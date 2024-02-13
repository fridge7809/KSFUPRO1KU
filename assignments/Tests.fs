
module Tests

open System.Runtime.InteropServices.Marshalling
open Xunit
open assignments

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