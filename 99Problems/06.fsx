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
  printfn "half is %d" half
  let index list =
    list |> List.mapi (fun index item -> (index,item)) //add index 
  
  let grabHalf list =
    let matching, _ = List.partition (fun indexItem -> (fst indexItem) < half) list
    matching |> List.map (fun indexItem -> snd indexItem)
  
  let hx = x |> index |> grabHalf
  let hxr = x |> List.rev |> index |> grabHalf 
  printfn "hx is %A" hx
  printfn "hxr is %A" hxr
  hx = hxr  
  
let isPalindromeLiam list =
  let rec popLast list =
    match list with
    | [] -> failwith "empty list"
    | [head] -> []
    | head::tail -> [head] @ popLast tail
  
  let rec last list =
    match list with
    | [] ->
      failwith "empty list!"
    | [head] -> head
    | head::tail ->
      last tail

  let rec isPalindrome list =
    match list with
    | [] -> true
    | [first] -> true
    | head::tail ->
      if head = last list then popLast tail |> isPalindrome
      else false
  
  isPalindrome list
  

let isPalindromeFromAnswerKey list = 
  list = List.rev list

let isPalindrome = isPalindromeFromAnswerKey


// let t1 = isPalindrome [1;2;3];;
// // val it : bool = false
// let t2 = isPalindrome <| List.ofSeq "madamimadam";;
// // val it : bool = true
// let t3 = isPalindrome [1;2;4;8;16;8;4;2;1];;
// // val it : bool = true
// let t4 = isPalindrome [1;2;2;1]
// // val it : bool = true

[
  isPalindrome [1;2;3],                    false
  isPalindrome (List.ofSeq "madamimadam"), true
  isPalindrome [1;2;4;8;16;8;4;2;1],       true
  isPalindrome [1;2;2;1],                  true
] 
//|> List.iter (printfn "expected = %A actual = %A") //partial application doesn't work because the incoming is a tuple
|> List.iter (fun (expected, actual) ->
    printfn "expected = %A actual = %A" expected actual
  )

