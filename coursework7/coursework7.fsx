(*

  ITT8060 -- Advanced Programming 2016
  Department of Computer Science
  Tallinn University of Technology
  ------------------------------------------------

  Coursework 7: Tail recursion and laziness

  ------------------------------------------------
  Name:
  Student ID:
  ------------------------------------------------


  Answer the questions below. You answers to the questions should be correct F#
  code written after the question. This file is an F# script file; it should be
  possible to load the whole file at once. If you can't, then you have
  introduced a syntax error somewhere.

  This coursework will be graded.

  Commit and push your script part of the solution to the repository as file
  coursework7.fsx in directory coursework7.

  Please do not upload DLL-s. Just include a readme.txt file containing the
  dependencies required (additional DLLs)

  The deadline for completing the above procedure is Friday, November 18, 2016.

  We will consider the submission to be the latest version of the appropriate
  files in the appropriate directory before the deadline of a particular
  coursework.

*)

(*
  Task 1:

  Write a function maxInList : int list -> int that returns the maximum element
  in the given list. Make sure your implementation uses tail recursion.
*)

let maxInList xs =
    let rec worker m = function
        | [] -> m
        | x::xs -> worker (max x m) xs
    worker 0 xs

(*
  Task 2:

  Write a function reverse :: 'a list -> 'a list that works like the function
  List.rev. Make sure your implementation uses tail recursion.
*)

let reverse xs = 
    let rec worker (acc: 'a list) (xs: 'a list)  : 'a list = 
        match xs with
        | [] -> acc
        | x::xs -> worker ([x]@acc) xs
    worker [] xs

(*
  Task 3:

  Below you find the definition of a type Tree of leaf-labeled trees. Write a
  function maxInTree : int Tree -> int that returns the maximum label in the
  given tree. Use continuation-passing style in your implementation.
*)

type 'a Tree =
  | Leaf   of 'a
  | Branch of 'a Tree * 'a Tree

let maxInTree tree =
    let rec worker (tree: 'a Tree) (cont: 'a -> 'a): 'a =
        match tree with
        | Leaf a -> cont a
        | Branch (a, b) -> worker a (fun x -> worker b (fun y -> cont (max x y)))
    worker tree id
        
(*
  Task 4:

  Write a function maxInTree' : int Tree -> int that returns the maximum label
  in the given tree, like the function maxInTree from Task 3 does. Use
  continuation-passing style in combination with accumulation in your
  implementation.
*)

let maxInTree' tree =
    let rec worker (tree: 'a Tree) (acc: 'a) (cont: 'a -> 'a): 'a =
        match tree with
        | Leaf a -> cont (max acc a)
        | Branch (a, b) -> worker a acc (fun x -> worker b x cont)
    worker tree 0 id

//let rec getLeftTree n tree =
//    if n>0
//    then Branch (getLeftTree (n-1) tree, Leaf (100000-n))
//    else tree
//
//let rec getRightTree n tree =
//    if n>0
//    then Branch (Leaf (100000-n), getLeftTree (n-1) tree)
//    else tree
//
//let long = getRightTree 60000 (getLeftTree 60000 (Leaf 180000))
//
//let tree = Branch(Branch(Leaf 14, Leaf 5), Leaf 3)
//maxInTree' tree
//
//maxInTree long
//maxInTree' long


(*
  Task 5:

  The function streamMap : ('a -> 'b) -> 'a Stream -> 'b Stream from the lecture
  is the stream analog of the function List.map. Write a function streamFilter :
  ('a -> bool) -> 'a Stream -> 'a Stream that is the stream analog of the
  function List.filter.
*)


type 'a Stream =
    | Stream of 'a * Lazy<'a Stream>
//
//let streamTail = function
//    | Stream (_, lxs) -> lxs.Value
//
//let rec from n = Stream (n, lazy from (n+1))
//
//let rec streamMap f xs = 
//    match xs with
//    | Stream (x, lxs) -> Stream (f x, lazy streamMap f lxs.Value)
//
//let squares = streamMap (fun x -> x*x) (from 1)
//
//let rec take n (Stream(x, xs)) =
//    if n>0 
//    then x::(take (n-1) xs.Value)
//    else []

let rec streamFilter f (Stream (x, lxs)) =
        if f x
        then Stream (x, lazy streamFilter f lxs.Value)
        else streamFilter f lxs.Value

//take 10 (streamFilter (fun x -> x%10 =0) squares)
