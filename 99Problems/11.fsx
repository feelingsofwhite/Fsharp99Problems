//http://www.fssnip.net/ao

// Modify the result of problem 10 in such a way that if an element has no duplicates it 
/// is simply copied into the result list. Only elements with duplicates are transferred as
/// (N E) lists.
///  
/// Example: 
/// * (encode-modified '(a a a a b c c a a d e e e e))
/// ((4 A) B (2 C) (2 A) D (4 E))
///  
/// Example in F#: 
/// 
/// > encodeModified <| List.ofSeq "aaaabccaadeeee"
/// val it : char Encoding list =
///   [Multiple (4,'a'); Single 'b'; Multiple (2,'c'); Multiple (2,'a');
///    Single 'd'; Multiple (4,'e')]

type 'a Encoding = Multiple of int * 'a | Single of 'a      //'//reset sublime's faulty syntax higlighting with 'generics

//....and..go..


let encodeModified leSequence =
  let leFold item acc =
    match acc with
    | (Multiple (count, char))::rest when char = item -> (Multiple (count + 1, char))::rest
    | (Single char)::rest when char = item -> (Multiple (2, char))::rest
    | _ -> (Single item)::acc

  List.foldBack leFold leSequence []

encodeModified <| List.ofSeq "aaaabccaadeeee"  |> printfn "results iz %A"