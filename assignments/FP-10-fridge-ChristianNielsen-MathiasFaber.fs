module assignments.FP_10

open System.Reflection.Metadata

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
    let half = xs.Length / 2
    match half with
    | half when half > 0 -> (List.take half xs, List.skip half xs)
    | _ -> (List.take 1 xs,[])
    
   
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
    | xs when xs.Length = 1 -> true
    | _ -> false
        
    
(* 2.2 declaring function merge *)
let rec merge (left, right) =
    match left, right with
    | [], right -> right
    | left, [] -> left
    | firstLeft::restLeft, firstRight::restRight ->
        if firstLeft <= firstRight then
            firstLeft :: merge (restLeft, right)
        else
            firstRight :: merge (left, restRight)


(* 2.3 declaring function divideAndConquer *)
let divideAndConquer split merge indivisible p =
    let rec dc p =
        if indivisible p then
            p
        else
            let left, right = split p
            merge ((dc left), (dc right))
    dc p


let testDivideAndConquer = divideAndConquer split merge indivisible [1; 2; 100; 200; 3; 9]


(* 11.2 *)
(* Find 11.2 in QueueWithMistakeFixed.java *)
