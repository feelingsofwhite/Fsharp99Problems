// Learn more about F# at http://fsharp.net
// See the 'F# Tutorial' project for more help.

//goal: every odd digit of a sin

let sin = "123 456 789"
let c = sin.[0]
let a = sin.ToCharArray()
let b = Seq.toList a
let d = Seq.toList sin

let c1:char = 'a'
let c2:char = 'b'

let nospace =
  //List.filter (fun c -> c != ' ')
  //List.filter (fun c -> not (c = ' ')) //huh.. char doesn't support !=
  List.filter (fun c -> c = ' ' |> not) //lol

//let include c = function
//  | [x] -> true
//  | x:remainder -> x


let rec oddeven = function
  | [] -> []
  | [x] -> [x]
  | x1::x2::xs -> 
        let nexty = oddeven xs
        x1::nexty

//open System; let x = "abcdef01234567" |> Seq.take 5 |> String.Concat;;

let daStuff r:string =    /// <-- methinks I meant to say (r:string) at the time...
  let o = 
      r |> 
      Seq.toList |>
      nospace |>
      oddeven |>
      List.toArray |>
      (fun charArray -> new System.String(charArray))
  o

//took a long break
  
let makeString (a: char list) = 
    List.fold (fun stringResultState char -> stringResultState + (string char) ) "" a

let makeString2 (a: char list) = 
    let seperator = ""
    a |> List.map string |> String.concat seperator 

let cheat (a: char list) = 
    let sb = new System.Text.StringBuilder()
    a |> List.iter (fun c -> sb.Append(c) |> ignore)
    sb.ToString()
    
let daNewStuff s =
    s
    |> Seq.toList     
    |> nospace     
    |> oddeven
    |> makeString

let asIsInOne s =
    s 
    |> Seq.toList
    |> List.filter (fun ch -> not(ch = ' '))
    |> oddeven
    |> (fun f -> f)
    |> List.map string
    |> String.concat ""



let inOne s = //you can do it all without a list
    s 
    |> Seq.filter (fun char -> not (char = ' '))
    |> Seq.mapi (fun index element -> (element, index)) //tuple!
    |> Seq.filter (fun (element, index) -> index % 2 = 0)
    //|> Seq.map fst
    //|> Seq.map string
    |> Seq.map (fst >> string)     
    |> String.concat ""

    //or
    //|> Seq.fold (fun stringResultState char -> stringResultState + (string (fst char)))  "" 
    //|> Seq.fold (fun stringResultState tuple -> tuple |> fst |> string |> (+) stringResultState )  "" 
    

    




[<EntryPoint>]
let main argv = 
    printfn "args %A" argv
    printfn "sin      %A" sin
    printfn "everyodd1 %A" (daNewStuff sin)
    printfn "everyodd2 %A" (asIsInOne sin)
    printfn "everyodd3 %A" (inOne sin)
    if (System.Diagnostics.Debugger.IsAttached) then
        System.Console.ReadKey() |> ignore
    0 // return an integer exit code
