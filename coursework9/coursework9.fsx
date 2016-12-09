(*

  ITT8060 -- Advanced Programming 2016
  Department of Computer Science
  Tallinn University of Technology
  ------------------------------------------------------------------------------

  Coursework 9: Asynchronous and reactive programming

  ------------------------------------------------------------------------------
  Name:
  Student ID:
  ------------------------------------------------------------------------------


  Answer the questions below. You answers to the questions should be correct F#
  code written after the question. This file is an F# script file; it should be
  possible to load the whole file at once. If you can't, then you have
  introduced a syntax error somewhere.

  This coursework will be graded.

  Commit and push your script part of the solution to the repository as file
  coursework9.fsx in directory coursework9.

  Please do not upload DLL-s. Just include a readme.txt file containing the
  dependencies required (additional DLLs)

  The deadline for completing the above procedure is Friday, December 9, 2016.

  We will consider the submission to be the latest version of the appropriate
  files in the appropriate directory before the deadline of a particular
  coursework.

*)

(*
  Task 1:

  Write a function downloadParallel : string list -> Async<string []> that takes
  a list of URLs and downloads the resources referenced by these URLs in
  parallel. Use the function downloadAsync from the lecture in your
  implementation.
*)

open System.IO
open System.Net

let ReadToEndAsync (reader: StreamReader) =
    Async.AwaitTask (reader.ReadToEndAsync())

let downloadAsync (url:string) = 
    async {
        printfn "before %s" url
        let req = HttpWebRequest.Create(url)
        use! resp = req.AsyncGetResponse()
        let stream = resp.GetResponseStream()
        use reader = new StreamReader(stream)
        printfn "after %s" url
        return! ReadToEndAsync reader
    }

let downloadParallel (urls: string list) =
    urls |> List.map downloadAsync |> Async.Parallel


//Async.RunSynchronously (downloadParallel ["http://whatismyip.akamai.com/"; "http://api.ipify.org/"; "https://api.ipify.org?format=json"])

(*
  Task 2:

  Write a function downloadSemiParallel : string list -> Async<string []> that
  takes a list of URLs and downloads the resources referenced by these URLs.
  Resources from URLs with the same domain name shall be downloaded
  sequentially, but otherwise, parallelism shall be used. The order of the
  resources in the resulting array can be chosen by you.
*)

let getHost x = 
    let uri = new System.Uri(x)
    uri.Host

// Here is the first way to solve this task.
// We take all possible different domains and download them in parallel. Repeat for the rest.
// Then we group this parallel blocks in one sequence
// for two urls (u1 and u2) from one domain and one (x1) from another it will look like this:
//   ___u1___
// _|        |___u2___
//  |___x1___|

let rec separateUnique urls u nu =
    match urls with
    | [] -> u, nu
    | x::xs -> if (u |> List.map getHost |> List.contains (getHost x))
               then separateUnique xs u (x::nu)
               else separateUnique xs (x::u) nu

let rec downloadSemiParallel urls = 
    let (u, nu) = separateUnique urls [] []
    async {
        match u with
        | [] -> return Array.empty
        | x -> 
            let! part = downloadParallel x
            let! other = downloadSemiParallel nu
            return (Array.concat [|part; other|])
    }
// end

// Here is the second way to solve this task.
// We take all urls from the same domain and download them sequentially
// Then we group this chains in one parallel block
// for two urls (u1 and u2) from one domain and one (x1) from another it will look like this:
//   ___u1______u2___
// _|                |_
//  |___x1____.......|

let rec getAsyncSequence group = 
    async {
        match group with
        | [] -> return Array.empty
        | x::xs ->
            let! part = downloadAsync x
            let! other = getAsyncSequence xs
            return Array.append [|part|] other
    }

let downloadSemiParallel' urls = 
    let domainGroups = List.groupBy <| getHost <| urls |> List.map (snd >> getAsyncSequence)
    async {
        let! all = Async.Parallel domainGroups
        return Array.concat all
    }

// as for me, second options is better (e.g. if x1 download time will be the same as u1+u2 first option will be longer)
// but both cases are faster than just sequential loading
   
// end

//let sampleUrls = ["http://api.ipify.org/"; "https://api.ipify.org?format=json"; "https://courses.cs.ut.ee/MTAT.03.244/2016_fall/uploads/Main/Module%209%20-%20Partnership"; "https://courses.cs.ut.ee/2016/softeco/fall/Main/Workshops"; "http://courses.cs.ut.ee"] 
//// link to the presentation should load slower than others, to make test more evident (at least try)
//sampleUrls |> downloadSemiParallel' |> Async.RunSynchronously |> ignore 
//sampleUrls |> downloadSemiParallel |> Async.RunSynchronously |> ignore

(*
  Task 3:

  Write an event stream additions : IObservable<string> that emits an event
  everytime a file is created in the current directory. Each such event shall
  carry the name of the created file.

  Furthermore, write an event stream removals : IObservable<string> that emits
  an event everytime a file is removed from the current directory. Each such
  event shall carry the name of the removed file.
*)

open System.IO

let watcher = new FileSystemWatcher(@".")
watcher.EnableRaisingEvents <- true

let additions = watcher.Created |> Observable.map (fun x -> x.Name)
let removals = watcher.Deleted |> Observable.map (fun x -> x.Name)

(*
  Task 4:

  Below you find the definition of a type Change whose values represent changes
  to a directory. Use the event streams from Task 3 to define an event stream
  changes : IObservable<Change> of all file additions and removals in the
  current directory.
*)

type Change =
  | Addition of string
  | Removal  of string

let changes = Observable.merge <| Observable.map Addition additions <| Observable.map Removal removals

//changes |> Observable.add (fun x -> 
//                                match x with 
//                                | Addition(y) -> System.Console.WriteLine("add "+y)
//                                | Removal(y) -> System.Console.WriteLine("del "+y)
//                          )

(*
  Task 5:

  Use the event stream changes from Task 4 to define an event stream
  turnover : IObservable<int> that tells at every file addition or removal how
  much the number of files in this directory has increased since the beginning
  (with negative numbers signifying a decrease). For example, if two files are
  added and one file is removed afterwards, there should be three events, that
  carry the numbers 1, 2, and 1, respectively.
*)

let turnover =
  changes |> Observable.scan (fun n x -> 
                                    match x with
                                    | Addition(_) -> n+1
                                    | Removal(_) -> n-1
                             ) 0

//turnover |> Observable.add (fun x -> System.Console.WriteLine(x))
