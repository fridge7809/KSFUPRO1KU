module assignments.FP_03

(*
Exercise 3.1 Write a function
downTo:int->int list
so that downTo n returns the n-element list [n; n-1; . . .; 1]. You must use if-then-else expressions to
define the function.
Secondly define the function downTo2 having same semantics as downTo. This time you must use pattern
matching.
*)

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

(*
Exercise 3.2 Write a function
removeOddIdx:int list->int list
so that removeOddIdx xs removes the odd-indexed elements from the list xs:
removeOddIdx [x0; x1; x2; x3; x4; ...] = [x0; x2; x4; ...]
removeOddIdx [] = []
removeOddIdx [x0] = [x0]
*)

let rec removeOddIdx xs =
    match xs with
    | [] -> xs
    | x :: xs when x % 2 = 0 -> x :: removeOddIdx xs
    | _ :: xs -> removeOddIdx xs

let list = [0..10]

let res = removeOddIdx list

(*
Exercise 3.3 Write a function
combinePair:int list->(int*int) list
so that combinePair xs returns the list with elements from xs combined into pairs. If xs contains an odd
number of elements, then the last element is thrown away
*)

let rec combinePair (xs:int list) : (int*int) list =
    match xs with
    | [] -> []
    | x :: y :: xs -> (x,y) :: combinePair xs
    | _ -> []
    
(*
Exercise 3.4 Solve HR, exercise 3.2.
The former British currency had 12 pence to a shilling and 20 shillings to a pound.
Declare functions to add and subtract two amounts, represented by triples (pounds, shillings, pence) of integers,
and declare the functions when a representation by records is used.
Declare the functions in infix notation with proper precedences,
and use patterns to obtain readable declarations.
*)

type amount = {
    pounds:int
    shillings:int
    pence:int
}

type tripleAmt = int * int * int

let (+.) ((p1,s1,d1):tripleAmt) ((p2,s2,d2):tripleAmt) =
    let pence = d1 + d2
    let shillings = s1 + s2 + pence / 12
    let pounds = p1 + p2 + shillings / 20
    (pounds, shillings % 20, pence % 12)
    
let (-.) ((p1,s1,d1):tripleAmt) ((p2,s2,d2):tripleAmt) =
    let pence = d1 - d2
    let shillings = s1 - s2 - (if pence < 0 then 1 else 0)
    let pounds = p1 - p2 - (if shillings < 0 then 1 else 0)
    (pounds, shillings % 20, pence % 12)

let (+) (a:amount) (b:amount) =
    let pence = a.pence + b.pence
    let shillings = a.shillings + b.shillings + pence / 12
    let pounds = a.pounds + b.pounds + shillings / 20
    {pounds = pounds; shillings = shillings % 20; pence = pence % 12}
    
let (-) (a:amount) (b:amount) =
    let pence = a.pence - b.pence
    let shillings = a.shillings - b.shillings - (if pence < 0 then 1 else 0)
    let pounds = a.pounds - b.pounds - (if shillings < 0 then 1 else 0)
    {pounds = pounds; shillings = shillings % 20; pence = pence % 12}
    
let add (a:amount) (b:amount) =
    a + b
   
let substract (a:amount) (b:amount) =
    a - b
    
let addTriple (a:tripleAmt) (b:tripleAmt) =
    a +. b
    
let substractTriple (a:tripleAmt) (b:tripleAmt) =
    a -. b

