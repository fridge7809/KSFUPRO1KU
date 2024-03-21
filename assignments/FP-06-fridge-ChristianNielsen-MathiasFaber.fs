module assignments.FP_06

(*
    Exercise 6.1 HR exercise 6.2
    Postfix form is a particular representation of arithmetic expressions where each operator is preceded by its operand(s), for example:
    (x + 7.0) has postfix form x 7.0 +
    (x+7.0)∗(x−5.0) has postfix form x7.0 + x5.0 − ∗
    Declare an F# function with type Fexpr -> string computing the textual, postfix form of expression trees from Section 6.2.
    
*)

type Fexpr = 
    | Const of float | X
    | Add of Fexpr * Fexpr
    | Sub of Fexpr * Fexpr
    | Mul of Fexpr * Fexpr
    | Div of Fexpr * Fexpr
    | Sin of Fexpr
    | Cos of Fexpr
    | Log of Fexpr
    | Exp of Fexpr

let rec toString = function
    | Const x -> string x
    | X -> "x"
    | Add(fe1,fe2) -> "(" + (toString fe1) + " " + (toString fe2) + " +" + ")"
    | Sub(fe1,fe2)  -> "(" + (toString fe1) + " " + (toString fe2) + " -" + ")" 
    | Mul(fe1,fe2)  -> "(" + (toString fe1) + " " + (toString fe2) + " *" + ")"
    | Div(fe1,fe2)  -> "(" + (toString fe1) + " " + (toString fe2) + " /" + ")"
    | Sin fe -> "(" + (toString fe) + ") sin"
    | Cos fe -> "(" + (toString fe) + ") cos"
    | Log fe -> "(" + (toString fe) + ") log"
    | Exp fe -> "(" + (toString fe) + ") exp"

// Test strings:
toString(Mul(Cos(Mul(X, X)), Add(Mul(Const 1.0, X), Mul(X, Const 1.0))));;
toString (Sin(Mul(X, Const 7.0)))


// Exercise 6.2 HR exercise 6.8

// 1
type Instruction = | ADD | SUB | MULT | DIV | SIN | COS | LOG | EXP | PUSH of float

type Stack = float list

let intpInstr (stack: Stack) (instr: Instruction) : Stack =
    match instr with
    | ADD ->
        match stack with
        | a::b::rest -> (b + a)::rest
        | _ -> failwith "ADD intruction failed"
    | SUB ->
        match stack with
        | a::b::rest -> (b - a)::rest
        | _ -> failwith "SUB intruction failed"
    | MULT ->
        match stack with
        | a::b::rest -> (b * a)::rest
        | _ -> failwith "MULT intruction failed"
    | DIV ->
        match stack with
        | a::b::rest -> (b / a)::rest
        | _ -> failwith "DIV intruction failed"
    | SIN ->
        match stack with
        | a::rest -> (sin a)::rest
        | _ -> failwith "SIN intruction failed"
    | COS ->
        match stack with
        | a::rest -> (cos a)::rest
        | _ -> failwith "COS intruction failed"
    | LOG ->
        match stack with
        | a::rest -> (log a)::rest
        | _ -> failwith "LOG intruction failed"
    | EXP ->
        match stack with
        | a::rest -> (exp a)::rest
        | _ -> failwith "EXP intruction failed"
    | PUSH x -> x::stack

// Test:
let myStack : float list = [3.0 .. 7.0]
intpInstr myStack ADD
intpInstr myStack MULT


// 2
let intpProg (instructions: Instruction list) : float =  
    let rec helper (stack: Stack) (instructions: Instruction list) : Stack = 
        match instructions with
        | [] -> stack
        | a :: rest -> 
            let updatedStack = intpInstr stack a
            helper updatedStack rest
    match helper [] instructions with 
    | [] -> failwith "Empty instruction list"
    | result :: _ -> result

// Test:
intpProg [PUSH 1.0; PUSH 2.0; PUSH 5.0; PUSH 6.0; ADD; MULT]


// 3 - traverse expresssion tree in post order.
//   for each node, generate the corresponding instruction and append it to the list.
let trans (fe: Fexpr, x: float) : Instruction list =
    let rec helper (fe: Fexpr) : Instruction list = 
        match fe with
        | Const x -> [PUSH x]
        | X -> [PUSH x]
        | Add(fe1,fe2) -> helper fe1 @ helper fe2 @ [ADD]
        | Sub(fe1,fe2) -> helper fe1 @ helper fe2 @ [SUB]
        | Mul(fe1,fe2) -> helper fe1 @ helper fe2 @ [MULT]
        | Div(fe1,fe2) -> helper fe1 @ helper fe2 @ [DIV]
        | Sin fe -> helper fe @ [SIN]
        | Cos fe -> helper fe @ [COS]
        | Log fe -> helper fe @ [LOG]
        | Exp fe -> helper fe @ [EXP]
    helper fe

// Exercise 6.3 HR exercise 7.2
// Signature and implementation is combined in the same file given the constraints of the exercise (only able to upload one file)

// Signature to be put in an .fsi

(*
module Complex
type RE = float
type IM = float
[<Sealed>]
type Complex =
    | Complex of RE * IM
    static member ( + )  : Complex * Complex -> Complex
    static member ( - )  : Complex * Complex -> Complex
    static member ( * )  : Complex  * Complex -> Complex
    static member ( * )  : Complex * Complex -> Complex
*)

// Implementation to be put in an .fs file
module Complex =

    type RE = float
    type IM = float
    type Complex =
        | Complex of RE * IM
        static member  (+) (c1: Complex, c2: Complex) : Complex =
            match c1, c2 with
            | Complex (re1, im1), Complex (re2, im2) -> Complex (re1 + re2, im1 + im2)
        static member  (-) (c1: Complex, c2: Complex) : Complex =
            match c1, c2 with
            | Complex (re1, im1), Complex (re2, im2) -> Complex (re1 - re2, im1 - im2)
        static member  (*) (c1: Complex, c2: Complex) : Complex =
            match c1, c2 with
            | Complex (re1, im1), Complex (re2, im2) -> Complex (re1 * re2 - im1 * im2, re1 * im2 + re2 * im1)
        static member (/) (c1: Complex, c2: Complex) : Complex =
            match c1, c2 with
            | Complex (re1, im1), Complex (re2, im2) -> 
                let denom = re2 * re2 + im2 * im2
                Complex ((re1 * re2 + im1 * im2) / denom, (re2 * im1 - re1 * im2) / denom)
