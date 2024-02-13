module assignments.FP_03

(*Exercise 3.1 Write a function
downTo:int->int list
so that downTo n returns the n-element list [n; n-1; . . .; 1]. You must use if-then-else expressions to
define the function.
Secondly define the function downTo2 having same semantics as downTo. This time you must use pattern
matching.*)

let rec downTo n =
    if n <= 0 then
        []
    else
        n :: downTo (n-1)

let rec downTo2 n =
    match n with
    | 0 -> []
    | _ -> n :: downTo2 (n-1)
    
// Homemade unit tests :)
let shouldEqual (x,y) =
    x = y
let shouldntEqual (x,y) =
    x <> y // structural inequaility
let positiveCase = (downTo 7, downTo2 7) |> shouldEqual
let negativeCase = (downTo 1, downTo2 4) |> shouldntEqual

(*Exercise 3.2 Write a function
removeOddIdx:int list->int list
so that removeOddIdx xs removes the odd-indexed elements from the list xs:
removeOddIdx [x0; x1; x2; x3; x4; ...] = [x0; x2; x4; ...]
removeOddIdx [] = []
removeOddIdx [x0] = [x0]*)

let rec removeOddIdx xs =
    match xs with
    | [] -> xs
    | x :: xs -> if x % 2 = 0 then removeOddIdx xs.Tail else removeOddIdx xs
    
let list = removeOddIdx [1..10]