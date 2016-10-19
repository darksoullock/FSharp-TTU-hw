// Here are some optional exercises, answer the questions below and
// make sure to write tests.

type Nat =
  | Zero
  | Suc of Nat

// 1. implement addition, multiplication, subtraction for Nat as
//    custom operators

let One = Suc Zero
let Two = Suc One

let rec add m n = 
   match m with
     | Zero   -> n
     | Suc m1 -> Suc (add m1 n) 

let rec sub m n = 
   match n with
     | Zero   -> m
     | Suc n1 -> 
        match m with
        | Zero -> failwith "The result is negative"
        | Suc m1 -> sub m1 n1          

let rec mul m n = 
   match n with
     | Zero   -> Zero
     | Suc n1 -> add m (mul m n1)


// 2. Write a converstion function from Nat to int

let rec toInt n = 
   match n with
     | Zero   -> 0
     | Suc n1 -> 1 + toInt n1

//toInt (sub (mul Two (add Two Two)) One)

// 3. Write an evaluator for the following language of aritmetic expressions:

type Exp =
  | Val of Nat
  | Add of Exp * Exp
  | Sub of Exp * Exp
  | Mult of Exp * Exp

let rec eval1 expr =
    match expr with
    | Val(n)    -> toInt n
    | Add(m,n)  -> (eval1 m) + (eval1 n)
    | Sub(m,n)  -> (eval1 m) - (eval1 n)
    | Mult(m,n) -> (eval1 m) * (eval1 n)

//    eval : Exp -> int

// 4. Extend the language and the evaluator to support Sub, and Mult

//Done

// 5. Write an evaluator for this language which has variables as well.

type Exp<'t> =
  | Val of Nat
  | Var of 't
  | Add of Exp<'t> * Exp<'t>

//    The evaluator should take an lookup function too:
//    eval : ('t -> int) -> Exp<'t> -> int

let rec eval<'t> (lookup: 't -> int) expr =
    match expr with
    | Val(n)    -> toInt n
    | Var(v)    -> lookup v 
    | Add(m,n)  -> (eval lookup m) + (eval lookup n)

//eval<int> (fun x -> x) (Add(Val(One), Var(2)))
    
// 6. Write a map function for Exp<'t>, it can be thought of a
//    'renaming' function that renames variables.

let rec mapE e f = 
    match e with
    | Val(n)    -> Val(n)
    | Var(v)    -> f v
    | Add(m,n)  -> Add(mapE m f, mapE n f)

//eval<int> (fun x -> x) (mapE (Add(Val(One), Var(2))) (fun x -> Val(Two)))

// 7. Write a bind function (see section 6.8.2) for Exp<'t>, it can be
//    thought of as a substitution function that replaces variables with
//    expressions.


let rec bind e (expMap:Map<'t, 'a>) = 
    match e with
    | Val(n)    -> Val(n)
    | Var(v)    -> Exp<'a>.Var(<@@ %%(expMap.Item v):int @@>)
    | Add(m,n)  -> Add(bind m expMap, bind n expMap)

//bind (Add(Val(One), Var("Two"))) (Map.ofList ["Two", <@@ 2+2-2 @@>])
