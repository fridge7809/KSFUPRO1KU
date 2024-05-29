// Question 1.1

type mymap<'a,'b> = MyMap of list<'a*'b>
let dice1 = [(1,4);(2,2);(3,3);(4,2);(5,2);(6,2)]
let dice2 = [(1,4);(2,2);(3,3);(4,3);(5,5);(6,3)]

// Ex1 is a list of (character, int) tuples and dice1 is a list of (int, int) tuples.

let ex1 = MyMap [('A',65);('B',66);('C',67)]

let emptyMap () = MyMap []

let size (m: mymap<'a, 'b>) : int =
    match m with
    | MyMap m -> List.length m
    
// Question 1.2

let isEmpty (m: mymap<'a,'b>) : bool =
    match (size m) with
    | 0 -> true
    | _ -> false
    
let tryFind k m : ('a * 'b) option when 'a : equality =
    match m with
    | MyMap m -> List.tryFind (fun (x,y) ->
        let x,_ = (x,y)
        match x with
        | x when x = k -> true
        | _ -> false) m
    
// when 'a : equality defines that type 'a must have structural equality
// the compiler needs it to verify that the type that may be supplied at runtime has equality operators
// because it is needed by the function

let remove (k: 'a) (m: mymap<'a, 'b>) : mymap<'a, 'b> when 'a : equality =
    match m with
    | MyMap m -> MyMap (List.filter (fun (x,y) ->
        let x,_ = (x,y)
        match x with
        | x when x = k -> true
        | _ -> false) m)
    
let add (k: 'a) (v: 'b) (m: mymap<'a, 'b>) : mymap<'a, 'b> when 'a : equality =
    let add m k v =
        match m with
        | MyMap m -> MyMap (List.append m [(k, v)])
    match tryFind k m with
    | Some (x, _) ->
        let map = remove x m
        add map k v
    | None -> add m k v
    
// Question 1.3

let upd (f: 'a -> 'a -> 'a) (k: 'a) (v: 'b) (m: mymap<'a,'b>) : mymap<'a, 'b> when 'a : equality =
    match tryFind k m with
    | Some (_, y) -> add k (f v y) m
    | None -> add k v m
    
let map (f: 'a -> 'b -> 'c) (m: mymap<'a,'b>) =
    let rec helper m1 list =
        match m1 with
        | MyMap m ->
            match m with
            | [] -> List.rev list
            | (x,y)::ys -> helper (MyMap ys) ((x, f x y)::list)
    helper m []
    
// fold
let fold f s m : 'a =
    let rec helper (s: 'a) (m: mymap<'a, 'b>) =
        match m with
        | MyMap m ->
            match m with
            | [] -> s
            | (x,y)::ys -> helper (f s x y) (MyMap ys)
    helper s m

// question 2.1

let even n = n % 2 = 0

let collatz n =
    match even n with
    | true -> n / 2
    | false -> (3 * n) + 1
    
let collatz' n =
    if n <= 0 then failwith "n is less or equal to 0"
    collatz n

// question 2.2

let applyN (f: 'a -> 'a) n N =
    let rec helper n N (acc: 'a list) =
        match N with
        | 0 -> List.rev acc
        | _ ->
            let res = f n
            helper res (N-1) (res::acc)
    helper n N [n]
    
let applyUntilOne (f: int -> int) n =
    let rec helper i acc =
        match i with
        | 1 -> acc
        | _ -> helper (f i) (acc+1)
    helper n 0

// Question 2.3

let rec mySeq f x =
    seq { yield x
          yield! mySeq f (f x) }
    
let hej = mySeq collatz 42

// mySeq collatz 42 returns a sequence expression that computes the sequence computed by the function f

let g x =
    x * 2

// Question 3.1

type name = string
type quantity = float
type date = int * int * int
type price = float
type transType = Buy | Sell
type transData = date * quantity * price * transType
type trans = name * transData
let ts : trans list =
    [("ISS", ((24,02,2014),100.0,218.99,Buy)); ("Lego",((16,03,2015),250.0,206.72,Buy));
    ("ISS", ((23,02,2016),825.0,280.23,Buy)); ("Lego",((08,03,2016),370.0,280.23,Buy));
    ("ISS", ((24,02,2017),906.0,379.46,Buy)); ("Lego",((09,11,2017), 80.0,360.81,Sell));
    ("ISS", ((09,11,2017),146.0,360.81,Sell)); ("Lego",((14,11,2017),140.0,376.55,Sell));
    ("Lego",((20,02,2018),800.0,402.99,Buy)); ("Lego",((02,05,2018),222.0,451.80,Sell));
    ("ISS", ((22,05,2018),400.0,493.60,Buy)); ("ISS", ((19,09,2018),550.0,564.00,Buy));
    ("Lego",((27,03,2019),325.0,625.00,Sell)); ("ISS", ((25,11,2019),200.0,680.50,Sell));
    ("Lego",((18,02,2020),300.0,720.00,Sell))]
    
let addTransToMap (t: trans) (m: Map<name, transData list>) : Map<name, transData list> =
    let name, transData = t
    match Map.tryFind name m with
    | Some list -> Map.add name (transData::list) m
    | None -> Map.add name [transData] m
    
let shares = List.foldBack addTransToMap ts Map.empty

// Questions 3.2

let accTrans (tq:float,avg:float) ((d,q,p,tType):transData) =
    match tType with
    | Buy -> (tq + q, (avg * tq + q * p)/(tq + q))
    | Sell -> (tq - q, avg)
    
let quantityAndAvgPrice ts =
    List.fold accTrans (0.0,0.0) ts
    
    
// map folding
let res =
    let shares = List.foldBack addTransToMap ts Map.empty
    Map.fold (fun acc k v -> Map.add k (quantityAndAvgPrice v) acc) Map.empty shares
    
// Question 4.1

// the function duplicates all list elements one time.
// For instance, given input list [e0; . . . ; en] the list returned by dup [e0; . . . ; en] is [e0;e0;e1;e1...en]

let dupA list =
    let rec helper list acc =
        match list with
        | [] -> List.rev acc
        | x::xs -> helper xs (x::x::acc)
    helper list []
    
// Question 4.2

let replicate2 i = Seq.init i (fun _ -> i)

let dupSeq = Seq.initInfinite (fun i -> i/2)

// Question 4.3
let lol = Seq.take 10 dupSeq
let rec dupSeq2 s =
    seq {
        let p = Seq.item 0 s
        yield p
        yield p
        yield! dupSeq2 (Seq.skip 2 s)
    }