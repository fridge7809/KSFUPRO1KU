module handin.FP_02

// Exercise 2.1
let timediff (hh1,mm1) (hh2,mm2) =
    let minutes1 = (hh1*60) + mm1
    let minutes2 = (hh2*60) + mm2
    abs (minutes1 - minutes2)
    
// Exercise 2.2
let minutes (hh,mm) =
    timediff (hh,mm) (00,00)
    
// Exercise 2.3 Solve HR, exercise 2.2 (CJ)

let rec pow (s:string, i:int) : string =
    match i with
    | 1 -> s
    | _ -> s + pow (s, i-1)

// Exercise 2.4 Solve HR, exercise 2.8 (CJ)

// Recusive

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

// homemade bespoke unit test relying on structural equality
let test expected actual =
    expected = actual
    
test (bin (4,2)) (alternativeBin (4,2))

// Exercise 2.5 Solve HR, exercise 2.9

// 1. Type is int * int -> int, in normal language a tuple (pair) of int that maps to an int
// 2. Terminates on condition x = 0, | (0,y) -> y
// 3. (2,3) -> (1,6) -> (0, 6) -> 6
// 4. f(x,y) calculates the product of consecutive integers from x down to 1 (base case), multiplying each integer with the running product y
