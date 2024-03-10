module assignments.FP_05

// Excercise 5.1

type 'a BinaryTree = 
    | Leaf
    | Node of 'a * 'a BinaryTree * 'a BinaryTree

let rec inOrder (tree: 'a BinaryTree) : 'a list = 
    match tree with
    | Leaf -> []
    | Node (v, l, r) -> (inOrder l) @ [v] @ (inOrder r)
    