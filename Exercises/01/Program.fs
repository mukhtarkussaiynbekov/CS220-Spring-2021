module CS220.Program // This line declares Program module.

open CS220.Library

// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

[<EntryPoint>] // This line is essential for a program as it defines the main entry point of this program.
let main argv =
    let result1 = prob1 1 1 1
    let result2 = prob2 "abc"
    let result3 = prob3 1.0 -2.0 1.0
    let result4 = prob4 8
    let result5 = prob5 2021 2
    printfn "%d" result1
    printf "%s" result2
    printfn "%f" result3
    printfn "%d" result4
    printfn "%d" result5
    0 // DON't touch this; this is an integer exit code meaning successful termination.
