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


// Exercise 8.3 HR exercise 9.10.
// You probably have to call with a larger number before you see the stack overflow, .e.g., try with bigListK
// 300000 id.
// Exercise 8.4 HR exercise 9.11.
// The functions count and countC are found on page 214 in HR.
// Exercise 8.5 HR exercise 11.1.
// Exercise 8.6 HR exercise 11.2.