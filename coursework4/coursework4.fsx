(*

  ITT8060 -- Advanced Programming 2016
  Department of Computer Science
  Tallinn University of Technology
  ------------------------------------

  Coursework 4: Higher order functions, option, list

  ------------------------------------
  Name:
  Student ID:
  ------------------------------------


  Answer the questions below. You answers to the questions should be
  correct F# code written after the question. This file is an F# script
  file; it should be possible to load the whole file at once. If you
  can't, then you have introduced a syntax error somewhere.

  This coursework will be graded.

  Commit and push your solution to the repository as file
  coursework4.fsx in directory coursework4.

  The deadline for completing the above procedure is Friday,
  October 21, 2016.

  We will consider the submission to be the latest version of the
  appropriate files in the appropriate directory before the deadline
  of a particular coursework.

*)

// 1. Write a function by pattern matching
// 
//   flattenOption : option<option<'a>> -> option<'a>
//
//   which squashes two layers of possible successes or failures into 1
//   E.g. Some Some 1 -> Some 1

let flattenOption = function
    | None -> None
    | Some x -> x

//flattenOption (Some (Some 2))
//flattenOption (Some(option<int>.None))
//flattenOption option<option<int>>.None


// 2. Can flattenOption by implemented using bind? If so, do it!

let flattenOptionBind v = Option.bind id v

//flattenOptionBind (Some (Some 2))
//flattenOptionBind (Some(option<int>.None))
//flattenOptionBind option<option<int>>.None

// 3. Write a function
//
//    idealist : list<option<'a>> -> list<'a>
//
//    which collects a list of possible successes or failures into a
//    list containing only the successes. Pay close attention to the type.

let rec idealist = function
    | [] -> []
    | None :: xs -> idealist xs
    | Some(x) :: xs -> x :: idealist xs

//idealist [(Some(1));(Some(2));None;(Some(4));None;(Some(6));None;None;(Some(9));(Some(0))]

// 4. Write a function
//
//    conservative : list<option<'a>> -> option<list<'a>>
//
//    that takes a list of possible successes or failures and returns
//    a list of successes if everything succeeded or returns failure
//    if 1 or more elements of the list was a failure. Again, pay
//    close attention to the type.

let conservative list =
    if List.contains(None) list
    then None
    else Some(list |> List.map(fun (Some(x)) -> x))

//[(Some(1));(Some(2));None;(Some(4));None;(Some(6));None;None;(Some(9));(Some(0))] |> conservative
//[1;2;4;5;7;8;9;0] |> List.map(fun x -> Some(x)) |> conservative


// 5. Write a function
//
//    chars : list<string> -> list<char>
//
//    This function should use List.collect (bind) and have the
//    following behaviour:
//    ["hello";"world"] -> ['h';'e';'l';'l';'o';'w';'o';'r';'l';'d']

let chars = List.collect(fun (x:string) -> List.ofArray (x.ToCharArray()))

//chars ["hello";"world"] 

// 6. Write a function
//
//    iprint : list<int> -> string
//
//    This function should use List.foldBack and have the following behaviour:
//    [1 .. 5] |-> "1,2,3,4,5,"

let iprint seq = List.foldBack (fun a b -> a.ToString() + "," + b) seq ""

//iprint [1..5]
