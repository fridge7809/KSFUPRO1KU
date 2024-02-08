module handin.FP_01

// Exercise 1.1 Write a function sqr:int->int so that sqr x returns x2.
let sqr (x:int) : int =
    x * x

// Exercise 1.2 Write a function pow : float -> float -> float so that pow x n returns xn.
let pow (x:float) (n:float) : float =
    x ** n


// 1.3 (1.1) Declare a function h: float * float -> float
let h (x:float, y:float) : float =
    System.Math.Sqrt (x**2 + y**2)
    
//1.4 (1.2) Write function expressions corresponding to the functions g and h in the exercises 1.1 and 1.2.

// g
fun n -> n + 4
// h
fun (x:float,y:float) -> System.Math.Sqrt (x**2 + y**2)

(*
1.5 (1.4) Declare a recursive function f: int -> int, where
f(n) = 1+2+···+(n−1)+n
for n ≥ 0. (Hint: use two clauses with 0 and n as patterns.)
State the recursion formula corresponding to the declaration. Give an evaluation for f(4).
*)
let rec f (n:int) : int =
    match n with
    | 0 -> n
    | _ -> + n + f (n - 1)
    
let evaluation = f 4 // evaulates to 10

(*
1.6 (1.5) The sequence F0 , F1 , F2 , . . . of Fibonacci numbers is defined by:
F0 = 0 F1 = 1
Fn = Fn−1 + Fn−2
Thus, the first members of the sequence are 0, 1, 1, 2, 3, 5, 8, 13, . . ..
Declare an F# function to compute Fn. Use a declaration with three clauses, where the patterns correspond to the three cases of the above definition.
Give an evaluations for F4.
*)
// todo fix
let rec fibonacci (n:int) : int =
    match n with
    | 0 -> n
    | 1 -> n
    | _ -> fibonacci(n - 1) + fibonacci(n - 2)
let fResult = f 4

(* 
1.7 (1.6) Declare a recursive function sum: int * int -> int, where
sum(m, n) = m + (m + 1) + (m + 2) + · · · + (m + (n − 1)) + (m + n)
for m ≥ 0 and n ≥ 0. (Hint: use two clauses with (m,0) and (m,n) as patterns.)
Give the recursion formula corresponding to the declaration.
*)
let rec sum (m:int,n:int) : int =
    match (m,n) with
    | m,0 -> m
    | m,n -> m + n + sum (m,n-1)
    
(*
1.8 (1.7) Determine a type for each of the expressions:
    (System.Math.PI, fact -1)
    fact(fact 4)
    power(System.Math.PI, fact 2)
    (power, fact)
*)

// (System.Math.PI, fact -1) is a tuple of (float * int)
// fact(fact 4) is an int
// power(System.Math.PI, fact 2) is a float
// (power, fact) is a (float * int -> float) * (int -> int):
// (float * float -> float) is a function taking a tuple of float and float as input and maps to a float
// (int -> int) is a function that takes an int and maps to an int

(*
1.9 (1.8) Consider the declarations:
      let a = 5;;
      let f a = a + 1;;
      let g b = (f b) + a;;
Find the environment obtained from these declarations and write the evaluations of the expressions f 3 and g 3.
*) 

// Environment obtained: 
// Environment: env = [ a -> 5 ]
// Evaluations: 
// f 3 = 4
// g 3 = 9

(*
Exercise 1.10 Write a function dup:string->string that concatenates a string with itself.
You can either use + or ˆ. For example:
val dup : string -> string
> dup "Hi ";;
val it : string = "Hi Hi "
*)

let dup (s:string) : string =
    s + s
    
(*
Exercise 1.11 Write a function dupn:string->int->string so that dupn s n creates the concatenation
of n copies of s. For example:
val dupn : string -> int -> string
> dupn "Hi " 3;;
val it : string = "Hi Hi Hi "
*)

let rec dupn (s:string) (n:int) : string =
    match n with
    | 1 -> s 
    | _ -> s + dupn s (n-1)