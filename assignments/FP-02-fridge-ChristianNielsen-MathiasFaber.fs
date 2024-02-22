module assignments.FP_02

// Exercise 2.1. Solution: subtract a from c and multiply by 60 to convert hours to minutes. Subtract b from d to get difference in minutes. Add these two to get total minutes difference. 
let timediff (a: int, b:int) (c:int, d:int) = 
    ((c-a)*60) + (d-b)

// Exercise 2.2. Solution: bind value (00,00) to midnight and call timediff with midnight and parameter
let minutes (a: int, b: int) = 
    a*60 + b
    
// Exercise 2.3 Solve HR, exercise 2.2 (CJ)
(*
Declare an F# function pow: string * int -> string, where: pow(s,n)=s·s···· ·s
where we use · to denote string concatenation. (The F# representation is +.)
*)
let rec pow (s:string, n:int) : string =
    match n with
    | i when i <= 0 -> s
    | 1 -> s
    | _ -> s + pow (s, n-1)

// Exercise 2.4 Solve HR, exercise 2.8 (CJ)

// Recursive
let rec bin (n,k) =
    match (n,k) with
    | (_,0) -> 1
    | (n,k) when n = k -> 1
    | (_,_) -> bin (n-1,k-1) + bin (n-1,k)

// Alternative 
let alternativeBin (n, k) =
    let rec fact n =
        match n with
        | 0 -> 1
        | n -> n * fact(n-1)
    let nFactorial = fact n
    let kFactorial = fact k
    nFactorial / (kFactorial * fact (n-k))


// Exercise 2.5 Solve HR, exercise 2.9

// 1. Type is int * int -> int, in normal language a tuple (pair) of int that maps to an int
// 2. Terminates on condition x = 0, | (0,y) -> y
// 3. (2,3) -> (1,6) -> (0, 6) -> 6
// 4. f(x,y) calculates the product of consecutive integers from x down to 1 (base case), multiplying each integer with the running product y

// Exercise 2.6 Solve HR, exercise 2.10

// 1. The type is bool * int -> int. Normal language: Tuple (pair) of bool and int that maps to an int
// 2. Because the interpreter evaluates the function eagerly, it will lead to an error where we enter
// enter infinite recursion as there is no case that matches negative numbers, even if the function isn't applied.
// 3. Unlike 2, we can short circuit evaluate the condition and we won't have to evaluate fact(-1).

// Exercise 2.7 Solve HR, exercise 2.13 (CJ)

let curry (f : 'a * 'b -> 'c) : 'a -> 'b -> 'c =
    fun x y -> f (x,y)

let uncurry (g : 'a -> 'b -> 'c) : 'a * 'b -> 'c =
    fun (x,y) -> g x y
