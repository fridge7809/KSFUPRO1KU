module assignments.FP_05

// Exercise 5.1

type 'a BinaryTree = 
    | Leaf
    | Node of 'a * 'a BinaryTree * 'a BinaryTree

let rec inOrder (tree: 'a BinaryTree) : 'a list = 
    match tree with
    | Leaf -> []
    | Node (v, l, r) -> (inOrder l) @ [v] @ (inOrder r)
    
// Exercise 5.2

let rec mapInOrder (f: 'a -> 'b) (t: 'a BinaryTree) : 'b BinaryTree =
    match t with
    | Leaf -> Leaf
    | Node (v, l, r) -> Node (f v, mapInOrder f l, mapInOrder f r)
    
// TODO
// Can you give an example of why mapInOrder might give a result different from mapPostOrder, but the
// result tree retruned in both cases is still the same.

// Exercise 5.3

let rec foldInOrder (f: 'a -> 'b -> 'b) (acc: 'b) (t: 'a BinaryTree) : 'b =
    match t with
    | Leaf -> acc
    | Node (v, l, r) -> foldInOrder f (f v (foldInOrder f acc l)) r

