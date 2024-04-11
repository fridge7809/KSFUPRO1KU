module assignments.FP_08

// Exercise 8.1 HR exercise 9.8.

type BinTree<'a> =
    | Leaf
    | Node of BinTree<'a> * 'a * BinTree<'a>
    
let rec count = function
      | Leaf          -> 0
      | Node(tl,_,tr) -> count tl + count tr + 1
      
let rec countA (acc: int) (tree: BinTree<'a>) =
    match tree with
    | Leaf          -> acc
    | Node(tl,_,tr) ->
        let leftCount = countA acc tl
        let rightCount = countA acc tr
        leftCount + rightCount + 1
    
let threeNodeBinTree = Node(Node(Leaf,1,Leaf),2,Node(Leaf,3,Leaf))

count threeNodeBinTree = countA 0 threeNodeBinTree // true
    
// Exercise 8.2 HR exercise 9.9.

let rec genTree xs =
      match xs with
      | [| |]   -> Leaf
      | [| x |] -> Node(Leaf,x,Leaf)
      | _   -> let m = xs.Length / 2
               let xsl = xs.[0..m-1]
               let xm  = xs.[m]
               let xsr = xs.[m+1 ..]
               Node(genTree xsl, xm, genTree xsr)
               
let t n = genTree [| 1..n |]

let t20000000 = t 20000000
               
let rec countC t c =
    match t with
    | Leaf          -> c 0
    | Node(tl,_,tr) ->
        countC tl (fun valueLeft -> countC tr (fun valueRight -> c(valueLeft + valueRight + 1)))
        
let rec countAC (tree: BinTree<'a>) (acc: int) (c: int -> 'b) : 'b =
    match tree with
    | Leaf          -> c 0
    | Node(tl,_,tr) ->
        countAC tl acc (fun vl -> countAC tr (acc + vl + 1) c)

let ac = countAC t20000000 0 id // todo fix


// Exercise 8.3 HR exercise 9.10.
                          
let rec bigListK n k =
          if n=0 then k []
          else bigListK ( n - 1) (fun res -> 1::k(res))
          
(*
val bigListK: n: int -> k: ('a list -> int list) -> int list

bigListK is a recursive function taking parameters n and k, building a list of 1 elements of length n using function k.
Cases:
    1. Base case:       n = 0 returns k of an empty list.
    2. Recursive case:  returns bigListK of the prior n and passes an anonymous function which prepends 1 to the k(res), 
                        with res being the accumulated result so far.
                        
The anonymous function essentially composes its results with a call to itself for every recursive call.

Stack overflow explained:
A naive memory analysis may think that the continuation is maintained it the heap.
However, given the cons operator (::), we must push a new stack frame and construct a new list for every recursive call.
This is because the previous stack maintains the closure for the function k (the 1 element).
Thus, the stack grows linearly to n O(n)
The closure for k is found by unwinding the recursion when meet the base case.

Evaluation trace of bigListK 4 id:

bigListK(3, k1) ->
bigListK(2, k2) ->
bigListK(1, k3) ->
bigListK(0, k4) -> k4([]) // base case met

k1(1::k2(1::k3(1::k4(1::[])))) ->       notice we must maintain a reference to prior stack frames 
                                        to obtain what we must prepend
x -> x (identity function)
Result: [1, 1, 1, 1]
*)

// "Correct" implementation
// Moving the cons operation to anonymous function to defer the operation down the call stack

let rec bigListKCorrect n k =
          if n=0 then k []
          else bigListK ( n - 1) (fun res -> k(1::res))

// Exercise 8.4 HR exercise 9.11.

let rec leftTree n =
    let rec helper n acc =
        match n with
        | 0 -> acc
        | n -> helper (n - 1) (Node(acc, n, Leaf))
    helper n Leaf

//leftTree 10


let rec rightTree n =
    let rec helper n acc =
        match n with
        | 0 -> acc
        | n -> helper (n - 1) (Node(Leaf, n, acc))
    helper n Leaf

//rightTree 10

// analyse count and countA functions with rightTree and leftTree
// count and countA can be found in (* 8.1 (9.8) *) from line 9 in this doc

let bigLeft: BinTree<int> = leftTree 100000

let bigRight: BinTree<int> = rightTree 100000
 
// running the functions with large left or right trees will hit the stack limit and crash 

// Comparing count and countA shows us that countA is faster (although, it is difficult to measure on fast computers)
//#time
let cA = countA 0 bigLeft
//#time
let c = count bigLeft

//#time
let ac2 = countAC bigRight 0 id
//#time
let cc = countC bigRight id
// again, difficult to measure time on fast computers, but it seems that countAC is faster than countC, because we use an accumulating parameter. 



// Exercise 8.5 HR exercise 11.1.
// Making a sequence of ALL positive odd numbers, then choosing the first 100 numbers and printing them.
let oddNumbers = Seq.unfold (fun state -> Some(state, state + 2)) 1
oddNumbers 
|> Seq.take 100 
|> Seq.iter (printfn "%d")



// Exercise 8.6 HR exercise 11.2.
// Making a sequence for all factorial numbers. Then printing the first 10 of them
let factorials = Seq.unfold (fun (n, f) -> Some(f, (n + 1, f * (n + 1)))) (0, 1)
factorials
|> Seq.take 10
|> Seq.iter (printfn "%d")