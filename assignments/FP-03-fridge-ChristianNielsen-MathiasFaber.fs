module assignments.FP_03

(*Exercise 3.1 Write a function
downTo:int->int list
so that downTo n returns the n-element list [n; n-1; . . .; 1]. You must use if-then-else expressions to
define the function.
Secondly define the function downTo2 having same semantics as downTo. This time you must use pattern
matching.*)

let rec downTo n =
    if n <= 0 then [] else n :: downTo (n - 1)

let rec downTo2 n =
    match n with
    | 0 -> []
    | _ -> n :: downTo2 (n - 1)

// Homemade unit tests :)
let shouldEqual (x, y) = x = y
let shouldntEqual (x, y) = x <> y // structural inequaility
let positiveCase = (downTo 7, downTo2 7) |> shouldEqual
let negativeCase = (downTo 1, downTo2 4) |> shouldntEqual

(*Exercise 3.2 Write a function
removeOddIdx:int list->int list
so that removeOddIdx xs removes the odd-indexed elements from the list xs:
removeOddIdx [x0; x1; x2; x3; x4; ...] = [x0; x2; x4; ...]
removeOddIdx [] = []
removeOddIdx [x0] = [x0]*)

let rec removeOddIdx (xs: list<int>) : list<int> =
    match xs with
    | [] -> []
    | [ x ] -> [ x ]
    | x :: _ :: xs' -> x :: removeOddIdx xs'

// 3.3
(*
    Write a function
    combinePair:int list->(int*int) list
    so that combinePair xs returns the list with elements from xs combined into pairs. If xs contains an odd
    number of elements, then the last element is thrown away:
    combinePair [x1; x2; x3; x4] = [(x1,x2);(x3,x4)]
    combinePair [x1; x2; x3] = [(x1,x2)]
    combinePair [] = []
    combinePair [x1] = []
*)

// first we check for an empty list or a list with only 1 element, and return an empty list in these cases
// if there are 2 elements or more, we take the two first elements, put them in a tuple, and call the function again.
// function returns a list of all the tuples. If theres an odd number of int's, the last number will be discarded.
let rec combinePair (xs: list<int>) : list<(int * int)> =
    match xs with
    | []
    | [ _ ] -> []
    | x :: y :: xs' -> (x, y) :: combinePair (xs')


// 3.4 (3.2)
(*
    The former British currency had 12 pence to a shilling and 20 shillings to a pound. 
    Declare functions to add and subtract two amounts, represented by triples (pounds, shillings, pence) of integers, and declare the functions when a representation by records is used. 
    Declare the func- tions in infix notation with proper precedences, and use patterns to obtain readable declarations.
*)
// We use a type of Amount to obtain readable declarations
// The functions for addition and subtraction is followed by example usage
type Amount =
    { Pounds: int
      Shillings: int
      Pence: int }

let (++) (a: Amount) (b: Amount) : Amount =
    { Pounds =
        a.Pounds
        + b.Pounds
        + (a.Shillings + b.Shillings + (a.Pence + b.Pence) / 12) / 20
      Shillings = (a.Shillings + b.Shillings + (a.Pence + b.Pence) / 12) % 20
      Pence = (a.Pence + b.Pence) % 12 }

let (--) (a: Amount) (b: Amount) : Amount =
    { Pounds =
        a.Pounds
        - b.Pounds
        - (a.Shillings - b.Shillings - (a.Pence - b.Pence) / 12 - 1) / 20
      Shillings = (a.Shillings - b.Shillings - (a.Pence - b.Pence) / 12 + 19) % 20
      Pence = (a.Pence - b.Pence + 12) % 12 }

let one: Amount =
    { Pounds = 10
      Shillings = 21
      Pence = 79 }

let two: Amount =
    { Pounds = 20
      Shillings = 39
      Pence = 87 }

let result1: Amount = one ++ two
let result2: Amount = two -- one


// 3.5 (3.3)
(*
    The set of complex numbers is the set of pairs of real numbers. Complex numbers behave almost like real numbers if addition and multiplication are defined by:
    (a,b)+(c,d) = (a+c,b+d) (a,b)·(c,d) = (ac−bd,bc+ad)
    1. Declaresuitableinfixfunctionsforadditionandmultiplicationofcomplexnumbers.
    2. Theinverseof(a,b)withregardtoaddition,thatis,−(a,b),is(−a,−b),andtheinverseof (a, b) with regard to multiplication, that is, 1/(a, b), is (a/(a2 + b2 ), −b/(a2 + b2 )) (provided that a and b are not both zero). Declare infix functions for subtraction and division of complex
    numbers.
    3. Uselet-expressionsinthedeclarationofthedivisionofcomplexnumbersinordertoavoid
    repeated evaluation of identical subexpressions.
*)
//Addition
let (+++) (a: float, b: float) (c: float, d: float) = (a + c, b + d)

// Multiplication
let (-*-) (a: float, b: float) (c: float, d: float) = (a * c - b * d, b * c + a * d)

//Subtraction
let (---) (a: float, b: float) (c: float, d: float) = (a - c, b - d)

//Division
let (-/-) (a: float, b: float) (c: float, d: float) =
    let denom = (c * c + d * d)
    let x = (a * c + b * d) / denom
    let y = (b * c - a * d) / denom
    (x, y)


// 3.6 (4.4)
(*
    Give a declaration for altsum (see Page 76) containing just two clauses.
*)
// Instead of checking for the empty list, we can start by checking for the two first elements in the list.
// if there are not two elements, it is clear that there is one or zero elements in the list, and using List.sum will then give us the sum of the list. (either 0 for an empty list or x for a list with one element x)
let rec altsum =
    function
    | x0::x1::xs -> x0 - x1 + altsum xs
    | lst -> List.sum lst
