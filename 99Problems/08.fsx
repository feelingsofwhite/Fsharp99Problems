//http://www.fssnip.net/an

// Problem 8 : Eliminate consecutive duplicates of list elements.


/// If a list contains repeated elements they should be replaced with a single copy of the 
/// element. The order of the elements should not be changed.
///  
/// Example: 
/// * (compress '(a a a a b c c a a d e e e e))
/// (A B C A D E)
///  
/// Example in F#: 
/// 
/// > compress ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"];;
/// val it : string list = ["a";"b";"c";"a";"d";"e"]


let compress list = 
  
  let foldBacker item acc  =
      //printfn "acc %A  item %A" acc item
      match acc with 
      | [] -> [item]
      | h::_ when h  = item -> acc
      | h::_ when h <> item -> item::acc
      
  List.foldBack foldBacker (List.rev list) [] |> List.rev

compress ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"] |> printfn "%A";;
/// val it : string list = ["a";"b";"c";"a";"d";"e"]
