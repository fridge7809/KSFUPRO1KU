module assignments.FP_04

open System.Xml.Serialization

(*
Exercise 4.1 
*)

let explode (s:string) =
    s.ToCharArray() |> List.ofArray
    
let rec explode2 (s:string) =
    match s with
    | "" -> []
    | _ -> s.Chars 0 :: explode2 (s.Remove(0, 1))
    
(*
Exercise 4.2
*)

let implode list =
    List.foldBack (fun c acc -> string c + acc) list ""

let implodeRev list =
    List.fold (fun acc c -> string c + acc) "" list
    
(*
Exercise 4.3
*)

let toUpper s =
    implode (List.map (fun c -> System.Char.ToUpper c) (explode s))
    
let toUpper1 s =
    (explode >> List.map (fun c -> System.Char.ToUpper c) >> implode) s
    
let toUpper2 s =
    s |> explode |> List.map (fun c -> System.Char.ToUpper c) |> implode
    

(*
Exercise 4.4
*)

let palindrome s =
    let reverse = s |> toUpper |> explode |> implodeRev
    s |> toUpper = reverse
    
(*
Exercise 4.5
*)

let rec ack (m,n) =
    match m,n with
    | m,n when (m < 0 || n < 0) -> failwith "The Ackermann function is defined for non negative numbers only"
    | m,n when m = 0 -> n + 1
    | m,n when (m > 0 && n = 0) -> ack (m-1, 1)
    | m,n when (m > 0 && n > 0) -> ack(m-1, ack(m,n-1))
    
let result = ack (3,11)

(*
Exercise 4.6
*)

let time f =
    let start = System.DateTime.Now in
    let res = f () in
    let finish = System.DateTime.Now in
    (res, finish - start)
    
time (fun () -> ack (3,11))

// 16381

let timeArg1 f a = time (fun () -> f (a))


(*
Excersise 4.7
*)
let rec fact = function
| 0 -> 1
| n when n > 0 -> n * fact(n-1)
| _ -> failwith "fact only works on positive numbers"

let rec downTo1 (f: 'a -> 'b -> 'c) n e =
    match n with
    | n when n > 0 -> f n (downTo1 f (n-1) e)
    | _ -> e
   
// curry fact to make it work with downTo1
// todo foldback to rev instead of rev
let buildList f n =
    let fact_curried n = f n in
    downTo1 (fun x acc -> (fact_curried x) :: acc) n []
    |> List.rev


//Downto1 alternative:
let rec downTo1Alternative f n e = 
    match n with
    | n when n <= 0 -> e
    | n -> downTo1Alternative f (n-1) e @ [n]

let buildListAlternative f n = 
    let list = downTo1Alternative f n []
    List.map(fun x -> f x) list // List.map maps all items in the list here to be called with function f

buildListAlternative fact 20
   