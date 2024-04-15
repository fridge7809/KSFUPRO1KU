module assignments.FP_09

// Question 1.1

type Heap<'a when 'a: equality> =
    | EmptyHP
    | HP of 'a * Heap<'a> * Heap<'a>
    
let ex3 = HP(1, HP (2, HP (3, EmptyHP, EmptyHP), HP (5, EmptyHP, EmptyHP)),
     HP (4, EmptyHP, EmptyHP))

// Type of ex3: Heap<int>

let empty = EmptyHP

exception HeapError of string

// Question 1.2

let isEmpty (hp : Heap<'a>) : bool =
    match hp with
    | EmptyHP -> true
    | _ -> false
    
let size (hp : Heap<'a>) : int = // todo fix this is wrong
    let n = 0
    let rec helper hp n =
        match hp with
        | EmptyHP -> n
        | HP(_,L, R) -> helper L (n+1) + helper R (n+1)
    helper hp n

size ex3