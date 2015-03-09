//http://www.fssnip.net/an

/// Transform a list, possibly holding lists as elements into a `flat' list by replacing each 
/// list with its elements (recursively).
///  
/// Example: 
/// * (my-flatten '(a (b (c d) e)))
/// (A B C D E)
///  
/// Example in F#: 
/// 

type 'a NestedList1 = List of 'a NestedList1 list | Elem of 'a
//aparantly syntactically the same as
//type NestedList2<'a> = List of NestedList2<'a> list | Elem of 'a

type 'a NestedList = 'a NestedList1
///
/// > flatten (Elem 5);;
/// val it : int list = [5]
/// > flatten (List [Elem 1; List [Elem 2; List [Elem 3; Elem 4]; Elem 5]]);;
/// val it : int list = [1;2;3;4;5]
/// > flatten (List [] : int NestedList);;
/// val it : int list = []


let flatten_implementation input = 
    let rec flatten_le_stuff state node = 
      match node with 
      | List l -> List.fold flatten_le_stuff state l
      | Elem e -> state @ [e]
    //flatten_le_stuff [] input

    //looked at solution 
    //and apparently this is how it should be because tail append is bad and head appeand is O(1) see http://stackoverflow.com/questions/7709632/why-dont-f-lists-have-a-tail-pointer
    let rec flatten_le_stuff_backwards node state = 
      match node with 
      | List l -> List.foldBack flatten_le_stuff_backwards l state 
      | Elem e -> e::state
    //flatten_le_stuff_backwards input [] 

    let rec flatten_le_stuff_collect node = 
      match node with 
      | List l -> List.collect flatten_le_stuff_collect l 
      | Elem e -> [e]
    flatten_le_stuff_collect input

    

let flatten listy =
    let result = flatten_implementation listy
    printfn "%A" result

flatten (Elem 5);;
//goal val it : int list = [5]
flatten (List [Elem 1; List [Elem 2; List [Elem 3; Elem 4]; Elem 5]]);;
//goal val it : int list = [1;2;3;4;5]
flatten (List [] : int NestedList);;
//goal val it : int list = []

