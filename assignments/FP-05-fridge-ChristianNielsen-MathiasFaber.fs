module assignments.FP_05

// Exercise 5.1

type 'a BinaryTree = 
    | Leaf
    | Node of 'a * 'a BinaryTree * 'a BinaryTree

let rec inOrder (tree: 'a BinaryTree) : 'a list = 
    match tree with
    | Leaf -> []
    | Node (v, l, r) -> (inOrder l) @ [v] @ (inOrder r)
    
// Exercise 5.2

let rec mapInOrder (f: 'a -> 'b) (t: 'a BinaryTree) : 'b BinaryTree =
    match t with
    | Leaf -> Leaf
    | Node (v, l, r) -> Node (f v, mapInOrder f l, mapInOrder f r)
    
// Exercise 5.3

let rec foldInOrder (f: 'a -> 'b -> 'b) (acc: 'b) (t: 'a BinaryTree) : 'b =
    match t with
    | Leaf -> acc
    | Node (v, l, r) -> foldInOrder f (f v (foldInOrder f acc l)) r

// Exercise 5.4 && Exercise 5.5

type aExp =                 (* Arithmetical expressions *)
    | N of int              (* numbers *)
    | V of string           (* variables *)
    | Add of aExp * aExp    (* addition *)
    | Mul of aExp * aExp    (* multiplication *)
    | Sub of aExp * aExp    (* subtraction *)
    | Inc of string
  
type bExp =                 (* Boolean expressions *)
    | TT                    (* true *)
    | FF                    (* false *)
    | Eq of aExp * aExp     (* equality *)
    | Lt of aExp * aExp     (* less than *)
    | Neg of bExp           (* negation *)
    | Con of bExp * bExp    (* conjunction *)
    
type stm =                      (* statements *)
    | Ass of string * aExp      (* assignment *)
    | Skip
    | Seq of stm * stm          (* sequential composition *)
    | IT of bExp * stm          (* If then *)
    | ITE of bExp * stm * stm   (* if-then-else *)
    | While of bExp * stm       (* while *)
    | RU of bExp * stm          (* Repeat until *)
    
type state = Map<string, int>

let update (x:string) (v:int) (s: state) : state = Map.add x v s
// val update: x: string -> v: int -> s: state -> state

let rec ArithmeticOperation (a: aExp) (s: state) : int =
    match a with
    | N n -> n
    | V x -> Map.find x s
    | Add(a1, a2) -> ArithmeticOperation a1 s + ArithmeticOperation a2 s
    | Mul(a1, a2) -> ArithmeticOperation a1 s * ArithmeticOperation a2 s
    | Sub(a1, a2) -> ArithmeticOperation a1 s - ArithmeticOperation a2 s
    | Inc i ->
        let res = Map.find i s
        res + 1
    
let rec BooleanOperation (b: bExp) (s: state) : bExp =
    match b with
    | TT -> TT
    | FF -> FF
    | Eq(aExp1, exp) ->
        let value1 = ArithmeticOperation aExp1 s
        let value2 = ArithmeticOperation exp s
        match (value1, value2) with
        | i, i1 when i = i1 -> TT
        | _,_ -> FF
    | Lt(aExp, exp) ->
        let value1 = ArithmeticOperation aExp s
        let value2 = ArithmeticOperation exp s
        match (value1, value2) with
        | i, i1 when i < i1 -> TT
        | _,_ -> FF
    | Neg bExp ->
        match (BooleanOperation bExp s) with
        | TT -> FF
        | FF -> TT
        | _ -> failwith "Syntax error"
    | Con(bExp, exp) ->
        let first = BooleanOperation bExp s
        let second = BooleanOperation exp s
        match (first, second) with
        | TT, TT -> TT
        | _,_ -> FF
        
let rec I (stm: stm) (s: state) =
    match stm with
    | Ass(x,a) -> update x (ArithmeticOperation a s) s
    | Skip -> s
    | Seq(stm, stm1) ->
        let newState = I stm s in
        I stm1 newState
    | ITE(bExp, stm, stm1) ->
        let eval = BooleanOperation bExp s
        if eval = TT then I stm s else I stm1 s
    | While(bExp, stm) ->
        let rec loop st =
            let eval = BooleanOperation bExp st
            if eval = TT then
                let newState = I stm st
                loop newState
            else
                st
        loop s
    | RU(bExp, stm) ->
        let rec loop st =
            let newState = I stm st
            let eval = BooleanOperation bExp s
            if eval = TT then I stm newState else newState
        loop s
    | IT(bExp, stm) ->
        match (BooleanOperation bExp s) with
        | TT -> I stm s
        | _ -> s

// Helper functions for testing
let assertEquals x y =
    x = y
let findValue v (s:state) =
    s.Item v
    
// Statements
let assign_6_to_x = Ass("x", Add(N 2, N 4))
let fact = Seq(Ass("y", N 1),While(Neg(Eq(V "x", N 0)),Seq(Ass("y", Mul(V "x", V "y")) ,Ass("x", Sub(V "x", N 1)))))
let assign_y_sub_100 = Ass("z", Sub(V "y", N 100))
let if_z_gt_620_assign_k_to_z_sub_y_else_assign_k_to_y = ITE(Neg(Lt((V "z", N 620))), Ass("k", Sub(V "z", V "y")), Ass("k", V "y"))
let assign_p_to_10_add_value_of_increment = Seq(Ass("p", N 10), Seq(Ass("o", N 3), While(Neg(Eq(V "o", N 0)), Seq(Ass("p", Add(V "o", V "p")), Ass("o", Sub(V "o", N 1))))))
let increment_x = Seq(Ass("x", N 0), Ass("x", Inc("x")))

// States
let empty: state = Map.empty
let s1 = I assign_6_to_x empty
let s2 = I fact s1
let s3 = I assign_y_sub_100 s2
let s4 = I if_z_gt_620_assign_k_to_z_sub_y_else_assign_k_to_y s3
let s5 = I assign_p_to_10_add_value_of_increment empty
let s6 = I increment_x empty

let result1 = (findValue "x" s1) = 6
let result2 = (findValue "y" s2) = 720
let result3 = (findValue "z" s3) = 620
let result4 = (findValue "k" s4) = -100
let result5 = (findValue "p" s5) = 16
let result6 = (findValue "x" s6) = 1

// Exercise 5.6
// Solution is implemented :)

(*
1. Add Inc as an ArithmeticOperation that finds the value of X in the current state and returns the value + 1
2. Inc(x) is performed when the interpreter matches Ass(x,a) and a is an Inc. The state is updated with the result of the statement.
*)