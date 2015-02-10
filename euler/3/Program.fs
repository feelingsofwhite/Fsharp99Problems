//
// The prime factors of 13195 are 5, 7, 13 and 29.
//
// What is the largest prime factor of the number 600851475143 ?
//


//explore FsCheck native (ie no xUnit.Net integration

open FsCheck

let revRevIsOrig (xs:list<int>) = List.rev(List.rev xs) = xs

Check.Quick revRevIsOrig

let revIsOrigBad (xs:list<int>) = List.rev xs = xs

Check.Quick revIsOrigBad




//boilerplate

[<EntryPoint>]
let main argv = 
    printfn "args are %A" argv
    if (System.Diagnostics.Debugger.IsAttached) then
        printfn "[press any key]"
        System.Console.ReadKey() |> ignore
    0 // return an integer exit code
