//http://www.fssnip.net/an

//#9 if a list contains repeated elements they should be placed in seperate sublists

//example
//pack  ['a'; 'a'; 'a'; 'a'; 'b'; 'c'; 'c'; 'a'; 'a'; 'd'; 'e'; 'e'; 'e'; 'e'; ]

// =  [['a'; 'a'; 'a'; 'a']; ['b']; ['c'; 'c']; ['a'; 'a']; ['d']; ['e'; 'e'; 'e'; 'e']; ]

let input = ['a'; 'a'; 'a'; 'a'; 'b'; 'c'; 'c'; 'a'; 'a'; 'd'; 'e'; 'e'; 'e'; 'e'; ]


let output = 
  let folder item state =
    match state with
    | [] 
    | [[]] -> [[item]]
    | head::tail -> match head with
                    | a::_ when a = item  -> (item::head)::tail
                    | _                   -> [item]::state

  List.foldBack folder input [[]]



let sawsolution = 
  let collect x = function                                // <- interesting, using partial application, or something like it, or basically whatever, the match is defined
    | (y::xs)::xss when x = y -> (x::y::xs)::xss
    | xss -> [x]::xss
                      
  List.foldBack collect input []


let sawsolution_with_names_more_like_my_original = 
  let folder item state =
    match state with
    | (a::b)::tail when item = a -> (item::a::b)::tail
    | _ -> [item]::state
                      
  List.foldBack folder input []

printfn "%A" sawsolution_with_names_more_like_my_original

