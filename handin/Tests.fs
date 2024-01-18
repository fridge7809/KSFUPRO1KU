
module Tests

open Xunit
open handin

module ``FP-01`` =
    
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
        