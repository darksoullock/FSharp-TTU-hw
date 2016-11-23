(*

  ITT8060 -- Advanced Programming 2016
  Department of Computer Science
  Tallinn University of Technology
  ------------------------------------------------------------------------------

  Coursework 8: Sequences and computation expressions

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
  coursework7.fsx in directory coursework7.

  Please do not upload DLL-s. Just include a readme.txt file containing the
  dependencies required (additional DLLs)

  The deadline for completing the above procedure is Friday, November 25, 2016.

  We will consider the submission to be the latest version of the appropriate
  files in the appropriate directory before the deadline of a particular
  coursework.

*)

(*
  Task 1:

  Define a sequence powers : int seq that contains all powers of 2 in ascending
  order. Use Seq.unfold in your implementation.
*)

let powers = Seq.unfold(fun x -> Some(x, x*2)) 2
//powers|> Seq.take 10 |> List.ofSeq

(*
  Task 2:

  Define a sequence primes : int seq that contains all prime numbers in
  ascending order. Use sequence expressions in your implementation. You may want
  to use the function isPrime : int -> bool defined below. This function checks
  whether the given number is a prime number.
*)

let isPrime n =
  let rec hasDivisorFrom d n =
    if d * d <= n then
      if n % d = 0 then
        true
      else
        hasDivisorFrom (d + 1) n
    else
      false
  not (hasDivisorFrom 2 n)

let primes =
    let rec from n =
        seq {
            if isPrime(n)
            then yield n
            yield! from (n + 2)
        }
    seq {
        yield! [2;3]
        yield! from 5
    }

//primes |> Seq.take 100 |> List.ofSeq

(*
  Task 3:

  Define a sequence primes' : int seq that again contains all prime numbers in
  ascending order. This time, do not use sequence expressions in your
  implementation, but use an appropriate function from the Seq module. Again,
  you may want to use the function isPrime : int -> bool defined above.
*)

let primes' = Seq.append (Seq.singleton 2) (Seq.unfold (fun x -> Some(x,x+2)) 3) |> Seq.filter isPrime 
//primes |> Seq.take 100 |> List.ofSeq

(*
  Task 4:

  Define a function fourthRoot : float -> float option that returns Some x if x
  is the 4th root of the argument, and None if the argument has no 4th root. In
  your implementation, use the squareRoot function from the lecture and
  computation expressions for the option type as defined in the lecture.
*)


let squareRoot x =
  if x >= 0.0 then Some (sqrt x) else None

type OptionBuilder () =
  member this.Bind   (opt, f) = Option.bind f opt
  member this.Return x        = Some x

let option = new OptionBuilder ()

let fourthRoot n =
    option {
        let! first = squareRoot n
        let! second = squareRoot first
        let! third = squareRoot second
        let! fourth = squareRoot third
        return fourth
    }

//fourthRoot 65536.0

(*
  Task 5:

  A function from a type 'env to a type 'a can be seen as a computation that
  computes a value of type 'a based on an environment of type 'env. We call such
  a computation a reader computation, since compared to ordinary computations,
  it can read the given environment. Below you find the following:

    • the definition of a builder that lets you express reader computations
      using computation expressions

    • the definition of a reader computation ask : 'env -> 'env that returns the
      environment

    • the definition of a function runReader : ('env -> 'a) -> 'env -> 'a that
      runs a reader computation on a given environment

    • the definition of a type Expr of arithmetic expressions

  Implement a function eval : Expr -> map <string, int> -> int that evaluates
  an expression using an environment which maps identifiers to values. Use
  computation expressions for reader computations in your implementation. Note
  that partially applying eval to just an expression will yield a function of
  type map <string, int> -> int, which can be considered a reader computation.
  This observation is the key to using computation expressions.
*)

type ReaderBuilder () =
  member this.Bind   (reader, f) = fun env -> f (reader env) env
  member this.Return x           = fun _   -> x

let reader = new ReaderBuilder ()

let ask = id

let runReader = (<|)

type Expr =
  | Const of int
  | Ident of string
  | Neg   of Expr
  | Sum   of Expr * Expr
  | Diff  of Expr * Expr
  | Prod  of Expr * Expr
  | Let   of string * Expr * Expr

let rec eval expr =
    reader {
        match expr with
        | Const(x)   -> return ask x
        | Ident(x)   -> let! a = Map.find x
                        return ask a
        | Neg(x)     -> let! a = eval x
                        return -a
        | Sum(x,y)   -> let! a = eval x
                        let! b = eval y
                        return a + b
        | Diff(x,y)  -> let! a = eval x
                        let! b = eval y
                        return a - b
        | Prod(x,y)  -> let! a = eval x
                        let! b = eval y
                        return a * b
        | Let(x,y,z) -> let! a = eval y
                        let! env = Map.add x a
                        let b = runReader (eval z) env
                        return ask b
    }

//let eval' expr (env:Map<string, int>) =
//    eval expr env

//let expr = Diff(Let("asd", Sum(Const(2),Const(2)), Prod(Ident("asd"), Const(5))), Ident("qwe"))   // (let asd = 2+2 in asd*5)-qwe
//let fn = runReader (eval expr) (Map.ofList ["qwe",3])
