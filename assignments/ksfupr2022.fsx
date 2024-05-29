// Question 1.1

type TrashItem<'a> =
    | Paper of string
    | Glass of string
    | Other of 'a

let item1 = Other ("Shirt", "Clothes")
let item2 = Paper "Newspaper"

// item1 is a TrashItem of a string pair (tuple)
// the compiler infers the type from the value provided

// item2 is a TrashItem of type string
// the compiler infers the type by matching the union case of the discriminated union

// TrashItem<'a> is polymorphic because it can be instantiated to any type that is provided by the caller

let items = [Paper "Magasine"; Glass "Bottle"; Glass "Jam"; Other ("Beer can", "Aluminium"); Other ("Bag", "Plastic")]

let fnPP (n, t) =
    let n, t = (n, t) // deconstruct pair
    $"{n} ({t})" // string interpolate
        
let ppTrashItem fnPP item =
    match item with // pattern match on the type of item and string interpolate or call supplied function if other
    | Paper p -> $"Paper ({p})"
    | Glass g -> $"Glass ({g})"
    | Other o -> fnPP o
    
(*ppTrashItem fnPP item1
val it: string = "Shirt (Clothes)"

ppTrashItem fnPP item2
val it: string = "Paper (Newspaper)"*)

let isPaper item =
    match item with // pattern match on the type
    | Paper _ -> true 
    | _ -> false // use wildcard to signal we dont need the value to the reader
    
// since its a DU and the cases are disjoint, it must be true that it isn't paper if we don't match on the paper case


// Question 1.2

type TrashCan<'a> =
    | Empty
    | TrashItems of TrashItem<'a> * TrashCan<'a>
    
let addItem item tc = TrashItems (item, tc)

let ofList ts =
    let rev = List.rev ts // we walk the list using recursion so we must first reverse the original list
    let rec helper list acc : TrashCan<'a> =
        match list with
        | [] -> acc // base case is the list is empty, then rewind recursion and return acc parameter
        | x::xs -> helper xs (addItem x acc) // recursively call helper function with tail, append head to trashcan
    helper rev Empty // call helper function
    
let forAll fnP tc =
    let rec helper tc =
        match tc with
        | Empty -> true // predicate is true for non-existing objects
        | TrashItems (a, tc) ->
            match fnP a with // match function result, recursive the rest of the tc if there are more items, return false if false
            | true -> helper tc
            | false -> false
    helper tc
    
let isSameCategory item1 item2 =
    match item1, item2 with
    | Paper _, Paper _' -> true
    | Glass _, Glass _' -> true
    | Other (_, c), Other (_', c') -> if c = c' then true else false
    | _ -> false
    // we only consider cases where item1 and item2 share a common type because all other cases must be false
    // deconstruct the tuple in the Other case
    // wildcard matches all other cases
    
let tcEx = ofList items

let isSorted tc =
    let a, tc = tc // get first item and bind to a
    let partial b =
        isSameCategory a b
    // my instinct says to curry isSameCategory to partially apply it before we use it in forAll
    // the other argument is supplied in forAll
    forAll partial tc
    
// Question 1.3
let tcEx = ofList items

let fold f e tc =
    let rec helper tc acc =
        match tc with
        | Empty -> acc
        | TrashItems (a, tc) -> helper tc (f acc a)
    helper tc e
    // define recursive helper method, return acc when we reach end of tc, otherwise call helper with tc and the result
    // of the mapping by the function as the new acc
    
    
let sort (tc: TrashCan<'a>) : (TrashCan<'a> * TrashCan<'a> * TrashCan<'a> ) =
    let paper = fold (fun s t ->
        match t with
        | Paper _ -> TrashItems (t, s)
        | _ -> s) Empty tc
    let glass = fold (fun s t ->
        match t with
        | Glass _ -> TrashItems (t, s)
        | _ -> s) tc Empty
    let other = fold (fun s t ->
        match t with
        | Other _ -> TrashItems (t, s)
        | _ -> s) tc Empty
    (paper, glass, other)
    // violates DRY but i dont have time to refactor :)
    // supply anon function that matches on type for fold, if it is of correct type then return
    
let (paperTc, glassTc, otherTc) = sort tcEx

let filter fnP tc =
    fold (fun state element ->
        match fnP element with
        | true -> TrashItems (element, state)
        | false -> state) Empty tc
    // supply function to folder that matches the result of the fnP on item
    
// Question 2.1

type Node<'a> =
    | Root of 'a
    | Link of 'a * Node<'a> ref
    
let mkRootElem a = ref (Root a)
let mkLinkElem a e = ref (Link(a,e))
let elemA = mkRootElem 'A'
let elemB =
    mkLinkElem 'B' elemA
    
let getVal e =
    let rec helper e =
        match e with
        | Root r -> r
        | Link(_, nodeRef) -> helper nodeRef.Value
    helper e
    
let pathLength e =
    let rec helper e acc =
        match e with
        | Root _ -> acc
        | Link(_, nodeRef) -> helper nodeRef.Value (acc+1)
    helper e 0
    
// Question 2.2

let find e 