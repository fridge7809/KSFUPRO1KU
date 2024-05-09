module assignments.FP_12

(*
IT UNIVERSITY KSD FUNCTIONAL PROGRAMMING SPRING 2024

Assignment 12

These exercises concern parallel programming in F#.

You should build on the files in the example code, found in folder
parProgExamples in the code repository, lecture 12.

Exercise 1. Run the slow Fibonacci computations from the lecture's
examples on your own computer, file parallelfib.fs.  Use the #time
directive to turn on timing, and see what is the best speed-up you can
obtain for computing, say, the first 43 Fibonacci numbers using
Async.Parallel.  This may be quite different on MS .NET than on Mono.
*)
open System.Threading.Tasks


let rec slowfib n = if n<2 then 1.0 else slowfib(n-1) + slowfib(n-2);;

// Don Syme examples, adapted
// Times are Mono 6.12.0 on MacOS 12.6.4 2,5 GHz Quad-Core Intel Core i7

// exersize 1:
#time // Real: 00:00:01.995, CPU: 00:00:03.073, GC gen0: 0, gen1: 0, gen2: 0
let fibs = 
    let tasks = 
        [for i in [1..42] do yield async { return slowfib(i) }]
    Async.RunSynchronously (Async.Parallel tasks)

(*
    Exercise 2. Similarly, run the prime factorization example on your own
    computer, and see what speedup you get, file primeFactors.fs.
*)
// prime factorization:
let factors n =
    let rec factorsIn d m =
        if m <= 1 then []
        else if m % d = 0 then d :: factorsIn d (m/d) else factorsIn (d+1) m
    factorsIn 2 n

// exersize 2:
#time // Real: 00:00:00.007, CPU: 00:00:00.004, GC gen0: 0, gen1: 0, gen2: 0
let primes = 
    let tasks = 
        [for i in [1..42] do yield async { return factors(i) }]
    Async.RunSynchronously (Async.Parallel tasks)


(*
Exercise 3. The lecture's construction of a histogram (counting the
numbers of times that each prime factor 2, 3, 5, 7, 11 ... appears)
uses a side effect in the assignment

     histogram.[i] <- histogram.[i] + 1 

But side effects should be avoided.  Program the histogram
construction so that it does not use side effects but purely
functional programming.  There are several useful functions in the Seq
module.  The final result does not have to be an

int[] array,

but could be a

seq<int * int>

of pairs (p, n) where p is a prime factor and n is the number of times
p appears in the array of lists of prime factors.
*)

let factors200000 = Array.Parallel.init 200000 factors;;

let factorsSeq = Seq.ofArray factors200000 |> Seq.collect id
let histogram =
    factorsSeq
    |> Seq.groupBy id
    |> Seq.map (fun (factor, group) -> factor, Seq.length group)

histogram;;

(**
Exercise 4. Find the fastest way on your hardware to count the number
of prime numbers between 1 and 10 million (the correct count is
664579).  Use this F# function to determine whether a given number n
is prime:

let isPrime n =
    let rec testDiv a = a*a > n || n % a <> 0 && testDiv (a+1)
    n>=2 && testDiv 2;;

or if you believe it would be faster in C, C# or Java, you may use this version:

private static boolean isPrime(int n) {
  int k = 2;
  while (k * k <= n && n % k != 0)
    k++;
  return n >= 2 && k * k > n;
}

Remember to use parallel programming to the extent possible.
*)

let isPrime n =
    let rec testDiv a = a*a > n || n % a <> 0 && testDiv (a+1)
    n>=2 && testDiv 2

let countPrimesInRange startNum endNum =
    [| startNum .. endNum |]
    |> Array.filter isPrime
    |> Array.length

let segmentSize = 1000000
let numSegments = 10

// by splitting the numbers from 0 to 10,000,000 up into 10 segments of numbers, we can run these 10 segments parallel and thereby faster
let countPrimesParallel() =
    let ranges =
        [ for i in 0 .. numSegments - 1 ->
            let startNum = i * segmentSize + 1
            let endNum' = (i + 1) * segmentSize
            let endNum = if endNum' > 10000000 then 10000000 else endNum'
            async { return countPrimesInRange startNum endNum }
        ]
    Async.Parallel ranges
    |> Async.RunSynchronously
    |> Array.sum

let result = countPrimesParallel()
printfn "Number of primes between 1 and 10 million: %d" result
