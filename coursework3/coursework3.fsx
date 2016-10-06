(*

  ITT8060 -- Advanced Programming 2016
  Department of Computer Science
  Tallinn University of Technology
  ------------------------------------

  Coursework 3: User defined types

  ------------------------------------
  Name:
  TUT Student ID:
  ------------------------------------


  Answer the questions below.  You answers to questions 1--7 should be
  correct F# code written after the question. This file is an F#
  script file, it should be possible to load the whole file at
  once. If you can't then you have introduced a syntax error
  somewhere.

  This coursework will be graded. It has to be submitted to the TUT
  git system using the instructions on the course web page by October 9, 2015.
*)

// 1. Consider expression trees of type ExprTree declared in the lecture.
// Extend the type with if-then-else expression of the form:
// if b then e1 else e2
// where b is a boolean expression and e1 and e2 are expressions.
// An example expression could be:
// if a+3 > b+c && a>0 then c+d else e

type BoolExprTree = 
    | Bool      of bool
    | Greater   of ExprTree * ExprTree
    | Less      of ExprTree * ExprTree
    | Equal     of ExprTree * ExprTree
    | And       of BoolExprTree * BoolExprTree
    | Or        of BoolExprTree * BoolExprTree
    | Not       of BoolExprTree
and ExprTree = 
    | Const     of int
    | Ident     of string
    | Minus     of ExprTree
    | Sum       of ExprTree * ExprTree
    | Diff      of ExprTree * ExprTree
    | Prod      of ExprTree * ExprTree
    | Let       of string * ExprTree * ExprTree
    | If        of BoolExprTree *  ExprTree * ExprTree 
    | Match     of ExprTree * ((ExprTree * ExprTree) list)


// if a+3 > b+c && a>0 then c+d else e
let ifExpr = If(
                And(
                    Greater(
                            Sum(Ident("a"), Const(3)),
                            Sum(Ident("b"),Ident("c"))),
                    Greater(Ident("a"),Const(0))), 
                Sum(Ident("c"), Ident("d")), 
                Ident("e"))

// 2. Extend the function eval defined in the lecture to support the
// if-then-else expressions defined in Q1.

let rec eval t env =
    match t with
    | Const n        -> n
    | Ident s        -> Map.find s env
    | Minus t        -> - (eval t env)
    | Sum (t1,t2)    -> eval t1 env + eval t2 env
    | Diff (t1,t2)   -> eval t1 env - eval t2 env
    | Prod (t1,t2)   -> eval t1 env * eval t2 env
    | Let (s,t1,t2)  -> let v1 = eval t1 env
                        let env1 = Map.add s v1 env
                        eval t2 env1
    | If (s,t1,t2)   -> if (evalBool s env) 
                        then eval t1 env
                        else eval t2 env
    | Match (value, list) ->
                        match list with
                        | [] -> 0
                        | (option, result)::tail -> if (eval option env) = (eval value env)
                                                    then eval result env
                                                    else eval (Match(value, tail)) env
and evalBool t env =
    match t with
    | Bool n         -> n
    | Greater (a, b) -> eval a env > eval b env
    | Less (a, b)    -> eval a env < eval b env
    | Equal (a, b)   -> eval a env = eval b env
    | And (a, b)     -> evalBool a env && evalBool b env
    | Or (a, b)      -> evalBool a env || evalBool b env
    | Not a          -> not (evalBool a env)

//let env : Map<string,int> = Map.ofList []

// test for if expression
//eval ifExpr (Map.ofList [("a", 1);("b", 1);("c", 3);("d", 4);("e", 5)])
//eval ifExpr (Map.ofList [("a", 2);("b", 1);("c", 3);("d", 4);("e", 5)])
//eval ifExpr (Map.ofList [("a", -2);("b", 1);("c", 3);("d", 4);("e", 5)])

// test for Match expression
//eval (Match(Const(2), [Const(4), Sum(Const(4),Const(1));Const(2), Sum(Const(4),Const(2))])) env
// match 2 with
// | 4 -> 4+1
// | 2 -> 4+2


// 3-4: Given the type definition:
type BList =
    | BEmpty
    | Snoc of BList * int
//
// 3. Make the function filterB: (prop: int -> bool) BList -> BList that will return a list for the elements of which
// the function prop returns true.

let rec filterB (prop: int -> bool) list = 
    match list with
    | BEmpty -> BEmpty
    | Snoc (tail, head) -> 
        let ftail = filterB prop tail 
        if prop head
        then Snoc (ftail, head) 
        else ftail 

//filterB (fun i -> i>0) (Snoc(Snoc(Snoc(Snoc(Snoc(Snoc(BEmpty,2),-3),4),-5),6),-7))

// 4. Make the function mapB: (trans: int -> int) BList -> BList that will return a list where the function trans has
// been applied to each element.

let rec mapB (prop: int -> int) list = 
    match list with
    | BEmpty -> BEmpty
    | Snoc (tail, head) -> Snoc (mapB prop tail, prop head)

//mapB (fun i -> i*i) (Snoc(Snoc(Snoc(Snoc(Snoc(Snoc(BEmpty,2),-3),4),-5),6),-7))

// 5-7. Given the type definition
// type Tree =
//  | Nil
//  | Branch2 of Tree * int * Tree
//  | Branch3 of Tree * int * Tree * int * Tree
//
// 5. Define the value exampleTree : Tree that represents the following
//    tree:
//
//        2
//       / \
//      *  3 5
//        / | \
//       *  *  *

type Tree =
  | Nil
  | Branch2 of Tree * int * Tree
  | Branch3 of Tree * int * Tree * int * Tree

let exampleTree = Branch2(Nil, 2, Branch3(Nil, 3, Nil, 5, Nil))

// 6. Define a function sumTree : Tree -> int that computes the sum of
//    all labels in the given tree.

let rec sumTree tree = 
    match tree with
    | Nil -> 0
    | Branch2 (b, a, c) -> a + (sumTree b) + sumTree c
    | Branch3 (c,a,d,b,e) -> a + b + (sumTree c) + (sumTree d) + sumTree e

//sumTree exampleTree

// 7. Define a function productTree : Tree -> int that computes the
//    product of all labels in the given tree. If this function
//    encounters a label 0, it shall not look at any further labels, but
//    return 0 right away.

let rec productTree tree = 
    match tree with
    | Nil -> 1
    | Branch2 (b, a, c) -> 
        if a=0 
        then 0 
        else a * (productTree b) * productTree c
    | Branch3 (c,a,d,b,e) -> 
        if a=0 || b=0 
        then 0 
        else a * b * (productTree c) * (productTree d) * productTree e

productTree exampleTree

// ** Bonus questions **

// 8. Extend the ExprTree type with a pattern match expression
// match p with [p1, ex1; p2,ex2 ...]

// Done

// 9. Extend the eval function to support match expressions.

// Done
