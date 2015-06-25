// https://projecteuler.net/problem=3

// The prime factors of 13195 are 5, 7, 13 and 29.
//
// What is the largest prime factor of the number 600851475143 ?
//

let isInt x = x |> int |> float |> (=) x;;

let isDivisor n x = isInt ((float n)/(float x))

let maybeDivisor n x = 
    if isInt ((float n)/(float x)) then 
      Some(x)
    else
      None

let numberstring = "600851475143"
let number = bigint.Parse numberstring

let divisors number = {(bigint 2)..number}

let tryDivisor number = 
  divisors number |> Seq.tryPick (maybeDivisor number)

let divisor number = 
  match tryDivisor number with
  | Some x -> x
  | None -> number

let rec listEm (acc: bigint list) (number: bigint) = 
  printfn "querying %A" number
  let asdf = tryDivisor number
  match asdf  with
  | _ when number = (bigint 1) -> printfn "no 1s babycakes"; acc
  | Some d -> printfn "d %A"           d ; listEm (d::acc) (number / d)
  | None   -> printfn "number %A" number ; number::acc

let primeFactors number = listEm [] number

printf "number %A, divisor %A" number (divisor number)

primeFactors number |> printfn "resulty %A"