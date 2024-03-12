
module Tests

open System.Runtime.InteropServices.Marshalling
open Xunit
open assignments
open assignments.FP_03
open assignments.FP_05

module ``FP-01 tests`` =
    
    open FP_01
    [<Fact>]
    let ``Should return sqr`` () =
        let expected = 36
        let actual = sqr 6
        Assert.Equal(expected, actual)
        
    [<Fact>]
    let ``Should return power of number`` () =
        let expected = 81 |> float
        let actual = pow 3 4
        Assert.Equal(expected, actual)
        
    [<Fact>]
    let ``h`` () =
        let expected = 5 |> float
        let actual = h (3,4)
        Assert.Equal(expected, actual)
        
    [<Fact>]
    let ``should duplicate string n times`` () =
        let expected = "Hi Hi Hi "
        let actual = dupn "Hi " 3
        Assert.Equal(expected, actual)
        
module ``FP-02 tests`` =
    
    open FP_02
    [<Fact>]
    let ``timediff first argument greater`` () =
        let expected = -59
        let actual = timediff (12,34) (11,35)
        Assert.Equal(expected, actual)
        
    [<Fact>]
    let ``timediff second argument greater`` () =
       let expected = 61
       let actual = timediff (12,34) (13,35)
       Assert.Equal(expected, actual)
       
    [<Fact>]
    let ``minutes since midnight`` () =
        let expected = 864
        let actual = minutes (14,24)
        Assert.Equal(expected, actual)
        
    [<Theory>]
    [<InlineData("elephants", 2, "elephantselephants")>]
    [<InlineData("apple", 3, "appleappleapple")>]
    [<InlineData("banana", 4, "bananabananabananabanana")>]
    [<InlineData("", 5, "")>]
    [<InlineData("dog", 0, "dog")>]
    [<InlineData("cat", -1, "cat")>]
    let ``Excercise 2.3`` (input:string, exponent:int, expected:string) =
        let actual = pow (input, exponent)
        Assert.Equal(expected, actual)
        
    [<Fact>]
    let ``Exercise 2.4`` () =
        let actual = bin 
        ()
        
module ``FP-03 tests`` =
    
    open FP_03
    [<Fact>]
    let ``Exercise 3.1`` () =
        let expected = [5;4;3;2;1]
        let actual = downTo 5
        Assert.Equal<int list>(expected, actual)
        Assert.Equal<int list>(downTo 5, downTo2 5)
        
    [<Fact>]
    let ``Exercise 3.2`` () =
        let expected = [2;4;6;8;10]
        let actual = removeOddIdx [1..10]
        Assert.Equal<int list>(expected, actual)
        
    [<Fact>]
    let ``Exercise 3.3`` () =
        let expected = [(1, 2); (3, 4); (5, 6)]
        let expected2 = [(1, 2); (3, 4)]
        let actual = combinePair [1;2;3;4;5;6]
        let actual2 = combinePair [1;2;3;4;5]
        Assert.Equal<(int*int) list>(expected, actual)
        Assert.Equal<(int*int) list>(expected2, actual2)

    [<Fact>]
    let ``Exercise 3.4`` () =
        let expected = Record { pounds = 7; shillings = 3; pence = 1 }
        let amount1 = Record { pounds = 4; shillings = 18; pence = 6 }
        let amount2 = Record { pounds = 2; shillings = 4; pence = 7 }
        Assert.Equal<Currency>(expected, amount1 + amount2)
    
    [<Theory>]
    [<InlineData(1, 2, 2, 4, 3, 6)>]
    [<InlineData(0, 0, 0, 0, 0, 0)>]
    [<InlineData(1, 1, 1, 1, 2, 2)>]
    let ``Exercise 3.5 addition`` (a, b, c, d, e, f) =
        let expected = Pair (e,f)
        let actual = (a, b) .+ (c, d)
        Assert.Equal<Pair>(expected, actual)
        
    [<Theory>]
    [<InlineData(5, 2, 2, 4, 2, 24)>]
    [<InlineData(10, 3, 0, 0, 0, 0)>]
    [<InlineData(1, 1, 1, 1, 0, 2)>]
    let ``Exercise 3.5 multiplication`` (a, b, c, d, e, f) =
        let expected = Pair (e,f)
        let actual = (a, b) .* (c, d)
        Assert.Equal<Pair>(expected, actual)
        
    [<Fact>]
    let ``Exercise 4.4`` () =
        let expected = -50
        let actual = altsum [1..100]
        Assert.Equal(expected, actual);
        
module ``FP-05 tests`` =
    
    open FP_05
    
    [<Fact>]
    let ``Exercise 5.1 tree is traversed in order`` () =
        let expected = [56; 25; 43; 562; 78]
        let tree = Node(43, Node(25, Node(56,Leaf, Leaf), Leaf),Node(562, Leaf, Node(78, Leaf, Leaf)))
        let actual = inOrder tree
        Assert.Equal<int list>(expected, actual)
        
    [<Fact>]
    let ``Exercise 5.2 tree is traversed in order and value is mapped`` () =
        let tree = Node(43, Node(25, Node(56,Leaf, Leaf), Leaf),Node(562, Leaf, Node(78, Leaf, Leaf)))
        let actual = mapInOrder (fun x -> x + 1) tree
        let expected = Node(44,Node (26,Node (57,Leaf,Leaf),Leaf),Node (563,Leaf,Node (79,Leaf,Leaf)))
        Assert.Equal<int BinaryTree>(expected, actual)
        
    [<Fact>]
    let ``Exercise 5.3 tree is traversed in order and value is mapped`` () =
        let tree = Node(43.0,Node(25.0, Node(56.0,Leaf, Leaf), Leaf),Node(562.0, Leaf, Node(78.0, Leaf,Leaf)))
        let actual = foldInOrder (fun acc x -> acc + x) 0.0 tree
        let expected = 764.0
        Assert.Equal<float>(expected, actual)
        