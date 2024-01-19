
module Tests

open System.Runtime.InteropServices.Marshalling
open Xunit
open handin

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
        let expected = 34
        let actual = timediff (12,26) (13,00)
        Assert.Equal(expected, actual)
        
    [<Fact>]
    let ``timediff second argument greater`` () =
       let expected = 155
       let actual = timediff (12,26) (15,01)
       Assert.Equal(expected, actual)
       
    [<Fact>]
    let ``minutes since midnight`` () =
        let expected = 62
        let actual = minutes (01,02)
        Assert.Equal(expected, actual)