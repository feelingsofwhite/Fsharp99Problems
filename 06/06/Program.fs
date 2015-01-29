open Swensen.Unquote
open Swensen.Unquote.Assertions
open Swensen.Unquote.Operators

// Learn more about F# at http://fsharp.net
// See the 'F# Tutorial' project for more help.

//http://www.fssnip.net/an

//(*) Problem 6 : Find out whether a list is a palindrome.
// 1: /// A palindrome can be read forward or backward; e.g. (x a m a x).
// 2: /// 
// 3: /// Example in F#: 
// 4: /// > isPalindrome [1;2;3];;
// 5: /// val it : bool = false
// 6: /// > isPalindrome <| List.ofSeq "madamimadam";;
// 7: /// val it : bool = true
// 8: /// > isPalindrome [1;2;4;8;16;8;4;2;1];;
// 9: /// val it : bool = true


let isPalindrome1 x = 
  let length = List.length x
  //let half = int (System.Math.Ceiling (float length / 2.0))
  let half = float length / 2.0 |> System.Math.Ceiling |> int
  //printfn "half is %d" half
  let index list =
    list |> List.mapi (fun index item -> (index,item)) //add index 
  
  let grabHalf list =
    let matching, _ = List.partition (fun indexItem -> (fst indexItem) < half) list
    matching |> List.map (fun indexItem -> snd indexItem)
  
  let hx = x |> index |> grabHalf
  let hxr = x |> List.rev |> index |> grabHalf 
  //printfn "hx is %A" hx
  //printfn "hxr is %A" hxr
  hx = hxr  
  

let isPalindrome2 _ = true

let isPalindrome = isPalindrome2
//
//test <@ let t1 = isPalindrome [1;2;3]
//        t1 = false @> // val it : bool = false
//
//test <@ let t2 = isPalindrome <| List.ofSeq "madamimadam"
//        t2 = true @> // val it : bool = true
//
//test <@ let t3 = isPalindrome [1;2;4;8;16;8;4;2;1]
//        t3 = true @> // val it : bool = true
//
//test <@ let t4 = isPalindrome [1;2;2;1]
//        t4 = true @> // val it : bool = true
//
true =? true

isPalindrome [1;2;3] =? false

(isPalindrome <| List.ofSeq "madamimadam") =? true

isPalindrome [1;2;4;8;16;8;4;2;1] =? true

isPalindrome [1;2;2;1] =? true


[<EntryPoint>]
let main argv = 
    //printfn "%A" argv
    if (System.Diagnostics.Debugger.IsAttached) then
      printfn "[press a key]"
      System.Console.ReadKey() |> ignore
    0 // return an integer exit code
