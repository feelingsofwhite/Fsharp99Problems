//
// https://projecteuler.net/problem=1
//
//PROBLEM
//
// If we list all the natural numbers below 10 that are multiples of 3 or 5, we get 3, 5, 6 and 9. The sum of these multiples is 23.
// Find the sum of all the multiples of 3 or 5 below 1000.
//

let max = 1000 //20 //10
let starters = [3; 5;]

let allPositives = 1 |> Seq.unfold (fun n -> Some(n, n + 1))

let limitedMultiples n = 
    let multiples n = seq { for i in allPositives do yield i * n }
    n |> multiples |> Seq.takeWhile (fun i -> i < max)

let answer = 
    starters 
    |> List.map limitedMultiples
    |> Seq.concat
    |> Seq.distinct              //fancier solution would be to write a custom zip, i think, instead of a concat & distinct, but this doesn't seem bad for max=1000
    |> Seq.sum
