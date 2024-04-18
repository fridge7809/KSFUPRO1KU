module assignments.FP_09

// Question 2.1

let random =
        let rnd = System.Random()
        fun () -> rnd.Next(1,1000)
        
let genRandoms (n:int) : int array =
    [| for _ in 1..n -> random () |] // array expression to generate array of random values from 1 to n
   
let genRandomsP (n:int) : int array =
    Array.Parallel.init n (fun _ -> random ())

// Question 2.2

let split (xs: 'a list) : ('a list * 'a list) =
    let half = (xs.Length / 2)
    if half = 0 then failwith "cannot split empty list"
    let takeHalf (l: 'a list) : 'a list =
        l |> List.take half
    (takeHalf xs, takeHalf xs)
    
// Test case 1: When splitting list with ood length, expect first list in tuple to be even and second list in tuple to be odd
let isEven n =
    match n with
    | n when n % 2 = 0 -> true
    | _ -> false
let case1 =
    match split [1..8] with
    | (x,y) ->
        assert (isEven x.Length)
        assert (not (isEven y.Length))

// Test case 2: When split list with even length, expect first list in tuple to be even and second list in tuple to be even
let case2 =
    match split [1..9] with
    | (x,y) ->
        assert (isEven x.Length)
        assert (isEven y.Length)
        
    
// Test case 3: list with 1 element is split into list containing element and empty list
let case3 =
    match split [1..1] with
    | (x,y) ->
        assert (x = [1])
        assert (y = []) // taking advantage of structural equality 
    
let indivisible (xs: 'a list) : bool =
    match xs with
    | xs when xs = [] -> true // true if empty
    | xs ->
        match split xs with
        | (x,_) when x.Length = 1 -> true // true if first list in tuple is of length 1, assumes test case 1 holds
        | _ -> false
        
// todo Declare a function merge xs ys of type

// todo Question 2.3
    
