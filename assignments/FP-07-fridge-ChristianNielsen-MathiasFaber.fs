
(* 7.1 (9.1)    
        Stack:                              Heap:

sf3      
        n       [0]             
        ys      []   
        result  [?]

sf2     n       [1]      
        ys      []              ->           (1, ) -> (points to xs from sf0)
        result  [?]

sf1     n       [2]
        ys      []              ->           (2, ) -> (2, ) -> (1, ) -> (1, x) dies
        result  [?]

sf0     xs                  ->               (1, ) -> (2, x)
        g                   ->               "closure for g"
        it                  ->               (1, ) -> (1, ) -> (2, ) -> (2, x)

*)



(* 7.2 (9.3) *)
(* declare an iterative solution to exercise 1.6 *)
// exercise 1.6
let rec sum (m: int, n: int) : int =
    match (m, n) with
    | m, 0 -> m
    | m, n -> m + n + sum (m, n - 1)

sum (10, 11)

// iterative version
let sumIterative (m: int, n: int) : int =
    let mutable result = m
    let mutable count = n

    while count > 0 do
        result <- result + m + count
        count <- count - 1

    result

sumIterative (10, 11)


(* 7.3 (9.4) *)

#time

let lengthIterative list =
    let mutable count = 0
    let mutable current = list

    while current <> [] do
        count <- count + 1
        current <- List.tail current

    count

lengthIterative [ 0..10 ]

(* 7.4 (9.6) *)
(*
    comparing the runtime of the continuation-based solution and the solution from Section 9.4
    shows us that the two solutions are both executed in less than a millisecond. 
*)

let rec factorialCont n cont =
    if n <= 1 then
        cont 1
    else
        factorialCont (n - 1) (fun result -> cont (n * result))

let factorial n = factorialCont n id

#time
factorial 30

let rec factA =
    function
    | (0, m) -> m
    | (n, m) -> factA (n - 1, n * m)

#time
factA (30, 1)



(* 7.5 (8.6) *)
(*
    Using mutable variables, and updating them continuously through the while loop
*)
let fibonacci n =
    if n <= 0 then
        failwith "Input must be positive."
    else
        let mutable fib1 = 0
        let mutable fib2 = 1
        let mutable i = 2

        while i <= n do
            let temp = fib1 + fib2
            fib1 <- fib2
            fib2 <- temp
            i <- i + 1

        fib2

fibonacci 5

(* 7.6 (9.7) *)