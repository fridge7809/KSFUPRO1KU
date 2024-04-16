module assignments.FP_09

// Question 1.1

type Heap<'a when 'a: equality> =
    | EmptyHP
    | HP of 'a * Heap<'a> * Heap<'a>
    
let ex3 = HP(1, HP (2, HP (3, EmptyHP, EmptyHP), HP (5, EmptyHP, EmptyHP)),
     HP (4, EmptyHP, EmptyHP))

// Type of ex3: Heap<int>

let empty : Heap<'a> when 'a: equality = EmptyHP

exception HeapError of string

// Question 1.2

let isEmpty (hp : Heap<'a>) : bool =
    match hp with
    | EmptyHP -> true
    | _ -> false
    
let rec size (h : Heap<'a>) : int =
    match h with
    | EmptyHP -> 0
    | HP(_,L,R) -> 1 + size L + size R

let find (h : Heap<'a>) : 'a when 'a: equality =
    // assumes the function is called from the root 
    match h with
    | HP(x, _, _) -> x
    | EmptyHP -> raise (HeapError("invalid tree"))

let rec checkHeapProperty (h : Heap<'a>) : bool when 'a: comparison =
    // helper method to check if root is less than or equal to rot of current node
    let helper x heap =
        match heap with
        | EmptyHP -> true
        | HP(root,_,_) -> x <= root
    // check property for current note and recursively for left and right children
    match h with
    | EmptyHP -> true
    | HP(r, h1, h2) -> helper r h1 && helper r h2 && checkHeapProperty h1 && checkHeapProperty h2
    
checkHeapProperty empty

// Question 1.3

let rec map (f: 'a -> 'b) (ha: Heap<'a>) : Heap<'b> when 'a: equality and 'b: equality =
    match ha with
    | EmptyHP -> EmptyHP
    | HP(foo, l, r) -> HP(f foo, map f l, map f r) // values are mapped using in-order traversal
    

let f a = a - (a*a) // substract a from a * a, as a grows larger it is gauranteed to break heap property
let res = checkHeapProperty (map f ex3) // val res: bool = false

// Question 2.1 - accidentally solved some of question 2

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
        

// came this far and then i realized i wasn't supposed to solve question 2 :)
    
// Question 3.1

let triNum : int seq =
    Seq.initInfinite (fun n -> (n * (n+1)) / 2)
    
let triNumC : int seq =
    triNum
    |> Seq.take 1000
    |> Seq.cache
    
// Question 3.2

let rec filterOddIndex s =
    Seq.append (Seq.singleton (Seq.item 0 s))
                (filterOddIndex (Seq.skip 2 s))
    
// Defer computation of element by putting it in a closure to avoid infinite recursion
let rec myFilterOddIndex s =
    Seq.delay (fun () ->
        let head = Seq.item 0 s
        Seq.append(Seq.singleton head)(myFilterOddIndex (Seq.skip 2 s)))
    

// Question 3.3

let rec seqZip (a: 'a seq) (b: 'b seq) : ('a * 'b) seq =
    seq {
        let e1 = Seq.item 0 a
        let e2 = Seq.item 0 b
        yield (e1, e2)
        yield! seqZip (Seq.skip 1 a) (Seq.skip 1 b)
    }