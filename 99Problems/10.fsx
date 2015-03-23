/// Use the result of problem P09 to implement the so-called run-length 
/// encoding data compression method. Consecutive duplicates of elements 
/// are encoded as lists (N E) where N is the number of duplicates of the element E.
///  
/// Example: 
/// * (encode '(a a a a b c c a a d e e e e))
/// ((4 A) (1 B) (2 C) (2 A) (1 D)(4 E))
///  
/// Example in F#: 
/// 
/// encode <| List.ofSeq "aaaabccaadeeee"
/// val it : (int * char) list =
///   [(4,'a');(1,'b');(2,'c');(2,'a');(1,'d');(4,'e')]


let combine item = function 
  | (innerHead::innerTail)::outerTail when innerHead=item -> (item::innerHead::innerTail)::outerTail
  | nomatchy -> [item]::nomatchy

let pack list = 
  List.foldBack combine list []

let runlengthInner item state =
  (List.length item, List.head item)::state

let runlength list =
  List.foldBack runlengthInner list []

let runLength_after_solution_of_course_use_map l =
  l |> List.map (fun x -> List.length x, List.head x)


List.ofSeq "aaaabccaadeeee" |> pack |> runLength_after_solution_of_course_use_map |> printfn "%A"