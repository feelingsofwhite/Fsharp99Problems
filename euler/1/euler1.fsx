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

//let multiply = 
//    Seq.unfold (fun n -> Some(n, n * n))
//
//let test = multiply 5
//
//Seq.head test
//test |> Seq.skip 2 |> Seq.head 

let allPositives = 1 |> Seq.unfold (fun n -> Some(n,n+1))

let multiples n = 
  seq {
         //invalid syntax: allPositives |> Seq.iter (fun i -> yield i * n)
         for i in allPositives do  
            yield i * n
  }

//so far so good
multiples 5 |> Seq.take 10 |> List.ofSeq 
multiples 3 |> Seq.take 10 |> List.ofSeq 

let multiples_terse n = seq { for i in (Seq.unfold (fun n -> Some(n,n+1)) 1) do yield i * n }

//okay, I would like to make a zip-like function which merges multiple sequences multiples iteratively, but screw it, brute force it with unique

let both_1 = Seq.concat    [(3 |> multiples |> Seq.takeWhile (fun i -> i < max));      (5 |> multiples |> Seq.takeWhile (fun i -> i < max))]
let both_2 = Seq.concat (starters |> List.map (fun r -> r |> multiples |> Seq.takeWhile (fun i -> i < max)))
let both_4 = starters |> List.map (fun s -> s |> multiples |> Seq.takeWhile (fun i -> i < max)) |> Seq.concat

let limitedMultiples n = n |> multiples |> Seq.takeWhile (fun i -> i < max)
let both_5 = starters |> List.map limitedMultiples |> Seq.concat

let both=both_5
both |> Seq.skip 9 |> Seq.take 1
both |> Seq.iter (fun i -> printf "%A, " i) 
Seq.concat

let uniques = Seq.distinct both

let answer = Seq.sum uniques

answer
