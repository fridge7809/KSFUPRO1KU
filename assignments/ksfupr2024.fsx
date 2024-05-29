// Question 1.1

let noRemainderP m n =
    match m % n = 0 with 
    | true -> true // return true if its true
    | _ -> false // use wildcard to say all other cases are false
    
let res1 = noRemainderP 10 2
let res2 = noRemainderP 10 3
// val res1: bool = true
// val res2: bool = false

let checkNumber n m =
    let rec helper m i =
        match noRemainderP m i with
        | true -> if i > 1 then noRemainderP m i else true // check i value to avoid recursing again which would cause a divide by zero
        | _ -> false
    helper m n
    
let res3 = checkNumber 10 2520
let res4 = checkNumber 11 2520
// val it: bool = true
// val it: bool = false
    
    
// i dont quite remember how to work with a variable that the anonymous function supplies but i would do something like this

(*let rec untilTrue f = function
    | n when f n -> n
    | n when f n <> true -> untilTrue (n - 1)*)
    
// passing a wildcard to the anonymous function would always evaluate to false
// since we essentially discard the value passed to the function and bind the function to false since it
// maps to false for all inputs
// thus, we enter infinite recursion and will run into a run time error or the program will hang
// if the function wasn't tail recursive then we would enter a stack overflow when we run out of memory that is allocated to the stack
    
// Question 1.2
    
let revAppend xs ys =
    let rec helper (xs: 'a list) (acc: 'a list) =
        match xs with
        | [] -> acc
        | x::xs -> helper xs (x::acc)
    let rev = helper xs []
    rev @ ys 

let res5 = revAppend [1;2] [3;4]
// val res5: int list = [2; 1; 3; 4]
        
let average xs =
    let rec helper xs (count: float) (sum: float) =
        match xs with
        | [] -> if count = 0.0 then 0.0 else sum / count
        | x::xs -> helper xs (count + 1.0) (sum + x)
    helper xs 0.0 0.0
    
// tail recursive helper function that increments count for each list element and keeps a running sum
// base case if an empty list, in which case we return sum / count or 0 (list is empty)
let res6 = average [1.0;2.0]
// val res6: float = 1.5
let res7 = average []
// val res7: float = 0.0

//let maxBy (f: 'a -> 'b) (xs: 'a list) : 'a when 'b : comparison =
    
// Question 1.3

let rec collect f = function
    | [] -> []
    | x::xs -> f x @ collect f xs
    
// collect is not tail recursive because each recursive call N relies on the prior call N-1
// In other words, the @ operator appends the result of a variable that is defined in a prior stack frame
// meaning the recursive call builds in length because it must contain pointers to all prior stack frames
// this can cause stack overflows on large lists

(*
let collect f = function
    | [] -> []
    | x::xs -> (f x) |> revAppend (collect xs)
*)
    
// doesn't quite work :(
    
// Question 2

type 'a diffList =
    | LISTS of 'a list list
    | SEQ of 'a diffList * 'a diffList
    
let exDF01 = SEQ (LISTS [[1;2];[3;4]],
        SEQ (LISTS [[];[5]],LISTS [[6;7];[8]]))
let exDF02 = SEQ (LISTS [[1;2];[3;4]], SEQ (LISTS [[5];[0]],LISTS [[6];[7;8]]))

let equals = exDF01 = exDF02

// val equals: bool = false

// we can use structural equality in a functional language to determine if the object are equal
// object oriented languages do not support this in the same way
// the default == in java for example compares if the variables have the same pointer to the heap for references types

let mkDiffList () : 'a diffList =
    LISTS []
    
// val mkDiffList: unit -> 'a diffList

let l1 : int diffList = mkDiffList()

let fromList xs =
    LISTS [[];[xs]]
    
    (* overengineered first solution
    let rec helper xs acc =
        match xs with
        | [] -> acc
        | x::y::xs -> helper xs (LISTS [[x;y]])
        | x::xs -> helper xs (LISTS [[x]])
    helper xs (LISTS [[]])*)
    
// val it: int list diffList = LISTS [[[1; 2]]; []]

let appendList xs diffList =
    let xsDiff = fromList xs // this call is O(1)
    SEQ (xsDiff, diffList) // by using the SEQ union case we can make a tuple of the diff lists after converting xs
    
// val it: int diffList = SEQ (LISTS [[1; 2]], LISTS [[3; 4]])

let append diffXs diffYs =
    SEQ (diffXs, diffYs) // creating a tuple is O(1)
    
// val it: int list diffList = SEQ (LISTS [[[1; 2]]; []], LISTS [[[3; 4]]; []])
        
// Question 2.2

(*
let flatten (diffXs: 'a diffList) =
    let rec helper (list: 'a diffList) (acc: 'a list) =
        match list with
        | LISTS (l::xs) -> l::(helper xs acc)
        | SEQ (a, b) ->
            match (a, b) with
            | _ -> helper a (helper b acc)
    helper diffXs []
    *)
    // doesnt quite work but my intention was to do in order walk/traversal of the tree structure to get a sorted list
    // by having a accumulating fsharp list parameter that we pre-pend to


// Question 3.1

type Currency = DKK | EUR | USD

type TransactionType = Deposit | Withdrawal

type Transaction = {
  date: int * int * int;
  transType: TransactionType;
  currency: Currency;
  amount: float }

type Account = {
  number: string;
  owner: string;
  transactions: Transaction list }

let exTransData01 = [(Deposit,    DKK, 45.5, (1,1,2024));
                     (Withdrawal, DKK, 90.0, (1,2,2024));
                     (Deposit,    USD, 10.0, (14,2,2024));
                     (Withdrawal, USD,  5.0, (14,1,2024));
                     (Deposit,    EUR, 42.0, (16,2,2024))]

let mkTrans t : Transaction =
    let ttype, c, f, t = t // deconstruct the input tuple
    let d, m, y = t
    { date = (d, m, y) ; transType = ttype ; currency = c ; amount = f }
    // not the prettiest variables names i have made i must admit
    
let exTrans01 = List.map mkTrans exTransData01

(*val exTrans01: Transaction list =
  [{ date = (1, 1, 2024)
     transType = Deposit
     currency = DKK
     amount = 45.5 }; { date = (1, 2, 2024)
                        transType = Withdrawal
                        currency = DKK
                        amount = 90.0 }; { date = (14, 2, 2024)
                                           transType = Deposit
                                           currency = USD
                                           amount = 10.0 };
   { date = (14, 1, 2024)
     transType = Withdrawal
     currency = USD
     amount = 5.0 }; { date = (16, 2, 2024)
                       transType = Deposit
                       currency = EUR
                       amount = 42.0 }]*)

let createAcc accNum owner transactions =
    { number = accNum ; owner = owner ; transactions = transactions }

let exAccount01 = createAcc "001" "Hans" exTrans01

(*val exAccount01: Account =
  { number = "001"
    owner = "Hans"
    transactions =
     [{ date = (1, 1, 2024)
        transType = Deposit
        currency = DKK
        amount = 45.5 }; { date = (1, 2, 2024)
                           transType = Withdrawal
                           currency = DKK
                           amount = 90.0 }; { date = (14, 2, 2024)
                                              transType = Deposit
                                              currency = USD
                                              amount = 10.0 };
      { date = (14, 1, 2024)
        transType = Withdrawal
        currency = USD
        amount = 5.0 }; { date = (16, 2, 2024)
                          transType = Deposit
                          currency = EUR
                          amount = 42.0 }] }*)

let addTransToAcc (account: Account) transaction =
    let list = account.transactions
    let newList = transaction::list
    { number = account.number ; owner = account.owner ; transactions = newList } // construct new account record with the new list
    
let exAccount02 =
    addTransToAcc exAccount01 (mkTrans (Deposit, USD, 10.0, (3,2,2024)))
    
(*val exAccount02: Account =
  { number = "001"
    owner = "Hans"
    transactions =
     [{ date = (3, 2, 2024)
        transType = Deposit
        currency = USD
        amount = 10.0 }; { date = (1, 1, 2024)
                           transType = Deposit
                           currency = DKK
                           amount = 45.5 }; { date = (1, 2, 2024)
                                              transType = Withdrawal
                                              currency = DKK
                                              amount = 90.0 };
      { date = (14, 2, 2024)
        transType = Deposit
        currency = USD
        amount = 10.0 }; { date = (14, 1, 2024)
                           transType = Withdrawal
                           currency = USD
                           amount = 5.0 }; { date = (16, 2, 2024)
                                             transType = Deposit
                                             currency = EUR
                                             amount = 42.0 }] }*)

let deposit account currency date amount =
    let toAdd = mkTrans (Deposit, currency, amount, date) // create transaction to add
    addTransToAcc account toAdd // return the account that is the result of the call to addTransToAcc
    
(*val it: Account =
  { number = "001"
    owner = "Hans"
    transactions =
     [{ date = (13, 2, 2024)
        transType = Deposit
        currency = DKK
        amount = 42.1 }; { date = (1, 1, 2024)
                           transType = Deposit
                           currency = DKK
                           amount = 45.5 }; { date = (1, 2, 2024)
                                              transType = Withdrawal
                                              currency = DKK
                                              amount = 90.0 };
      { date = (14, 2, 2024)
        transType = Deposit
        currency = USD
        amount = 10.0 }; { date = (14, 1, 2024)
                           transType = Withdrawal
                           currency = USD
                           amount = 5.0 }; { date = (16, 2, 2024)
                                             transType = Deposit
                                             currency = EUR
                                             amount = 42.0 }] }*)

// Question 3.2

type Balances = Map<Currency, float>

let getBalance (currency: Currency) (balances: Balances) =
    let res =
        match currency with
        | DKK -> balances.TryFind(DKK)
        | EUR -> balances.TryFind(EUR)
        | USD -> balances.TryFind(USD)
    match res with
    | Some res -> res
    | None -> 0.0
    
(*
getBalance DKK (Map [(DKK, 42.0);(USD, 43.25)])
val it: float = 42.0*)
    
(*
getBalance DKK (Map [])
val it: float = 0.0
    *)
    
let addToBalance (balances: Balances) values =
    let c, t, a = values
    let cur =
        match t with
        | Deposit -> (getBalance c balances) + a
        | Withdrawal -> (getBalance c balances) - a
    balances.Add(c, cur)
    
(*
addToBalance Map.empty (DKK,Deposit,100.0)
val it: Map<Currency,float> = map [(DKK, 100.0)]
*)

(*
addToBalance Map.empty (USD,Withdrawal,100.0)
val it: Map<Currency,float> = map [(USD, -100.0)]
*)

let getBalancesOfAccount (account: Account) =
    let trans = account.transactions
    let init = Map.empty
    // my approach would be to fold other map contents and reduce all transaction entries to be a sum of values that are mapped from the same key
    Map.fold (fun acc k (v: Transaction) -> Map.add k (addToBalance k (v.currency, v.transType, v.amount)) Map.empty)
    // didnt have time to complete

// Question 3.3

let conversionRates =
   Map [(USD, Map [(EUR, 0.93); (DKK, 6.94)]);
        (EUR, Map [(USD, 1.07); (DKK, 7.46)]);
        (DKK, Map [(USD, 0.14); (EUR, 0.13)])]
   
let getConvRate fromCurrency toCurrency (rates: Map<Currency,Map<Currency,float>>) =
    // convert currency to string representation for error handling
    let toString cur =
        match cur with
        | DKK -> "DKK"
        | USD -> "USD"
        | EUR -> "EUR"
        
    match rates.TryFind(fromCurrency) with
    | Some v ->
        match v.TryFind(toCurrency) with
        | Some v -> v
        | None -> failwithf $"no rates found from %s{toString fromCurrency} to %s{toString toCurrency}"
    | None -> failwithf $"no rates found from %s{toString fromCurrency} %s{toString toCurrency}"
    // use try find so we can match on the optional return value for easy error handling
    // use string interpolation for the error messages
   
let convertFunds (account: Account) fromCurrency toCurrency rates =
    let helper (t: Transaction) =
        let rate = if t.currency = fromCurrency then (getConvRate fromCurrency toCurrency rates) else 1.0
         // rate is 0 (unchanged) or get the rate from the map
        { date = t.date ; transType = t.transType ; amount = (t.amount * rate); currency = toCurrency } // construct new transaction record
    let res = account.transactions |> List.map helper // pipe transactions into mapper that applies helper function on all entries
    { number = account.number ; owner = account.owner ; transactions = res } // cconstruct new account record with the result
    
(*
let exAccount03 = convertFunds exAccount01 USD EUR conversionRates
val exAccount03: Account =
  { number = "001"
    owner = "Hans"
    transactions =
     [{ date = (1, 1, 2024)
        transType = Deposit
        currency = EUR
        amount = 45.5 }; { date = (1, 2, 2024)
                           transType = Withdrawal
                           currency = EUR
                           amount = 90.0 }; { date = (14, 2, 2024)
                                              transType = Deposit
                                              currency = EUR
                                              amount = 9.3 };
      { date = (14, 1, 2024)
        transType = Withdrawal
        currency = EUR
        amount = 4.65 }; { date = (16, 2, 2024)
                           transType = Deposit
                           currency = EUR
                           amount = 42.0 }] }


*)
    
    
let exAccount03 = convertFunds exAccount01 USD EUR conversionRates

        
// Question 4

let fizzBuzz n = function
    | n when n % 5 = 0 && n % 3 = 0 -> "FizzBuzz"
    | n when n % 3 = 0 -> "Fizz"
    | n when n % 5 = 0 -> "Buzz"
    | n -> $"{n}" // use string interpolation for a string interprotation of n

(*fizzBuzz 55
val it: string = "Buzz"
*)

(*fizzBuzz 7
val it: string = "7"
 *)
 
let fizzBuzzSeq = Seq.initInfinite fizzBuzz

// Seq.take 4 fizzBuzzSeq
// val it: seq<string> = seq ["FizzBuzz"; "1"; "2"; "Fizz"]

// We can instantiate it by suppling the fizzBuzz function to Seq.initInfite for lazy evaluation and defering the computation
// until it is needed for good performance
let fizzBuzzSeq2 =
    fizzBuzzSeq
    |> Seq.mapi (fun i e ->
        match e with
        | "Fizz" | "Buzz" | "FizzBuzz" -> (i, e)
        | _ -> (i, "toremove"))
    |> Seq.filter (fun i ->
        let i,e = i
        if e = "toremove" then false else true)

// We map all values to a tuple of (index,value) if we match on the string
// else add a tuple we can filter out later
// using the pipe operator to control the flow of the parameters for ease of reading
// I somehow broke it last second but it did work for a while

(*
val it: seq<int * string> =
  seq [(0, "FizzBuzz"); (3, "Fizz"); (5, "Buzz"); (6, "Fizz")]
  *)