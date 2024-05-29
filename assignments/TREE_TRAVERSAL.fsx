

(*
Look at the order of concatenation order in the different traversal methods:

Pre: x :: (preorder tl) @ (preorder tr)

    x : current value
    preorder tl : visit left tree
    preorder tr : visit right tree

In: (inorder tl) @ [x] @ (inorder tr)

    inorder tl : visit left tree
    x : current value
    inorder tr : visit right tree

Post: (postorder tl) @ (postorder tr) @ [x]

    postorder tl : visit left tree
    postorder tr : visit right tree
    x : current value

If you trace around your tree anti-clockwise starting at the top (above the root):

    Pre-order traversal returns the elements in the order of where you encounter the left-hand side of each node first.
    In-order traversal returns the elements in the order of where you encounter the bottom of each node first.
    Post-order traversal returns the elements in the order of where you encounter the right-hand side of each node first.

As a brief overview, pre-order traversal is useful for duplicating entire trees, in-order travel is useful for binary searching and post-order traversal is useful for deletion of entire trees.*)