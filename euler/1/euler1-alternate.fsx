//
// https://projecteuler.net/problem=1
//
//PROBLEM
//
// If we list all the natural numbers below 10 that are multiples of 3 or 5, we get 3, 5, 6 and 9. The sum of these multiples is 23.
// Find the sum of all the multiples of 3 or 5 below 1000.
//

//seen in the wild, yeah, i guess i coulda used mod instead of what i did lol

let max = 1000 //20 //10

[1..max-1]
  |> List.filter (fun i -> (i % 3 = 0) || (i % 5 = 0))
  |> List.sum


//someone else wrote this one
let get3or5seq max = seq { 
    for i in 1..(max-1) do 
        if i % 3 = 0 then yield i
        elif i % 5 = 0 then yield i
    }

let sum = Seq.fold (+) 0 (get3or5seq 1000)

printfn "%d" sum