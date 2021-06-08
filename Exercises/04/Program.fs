module CS220.Program // This line declares Program module.

open CS220.Library

// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

[<EntryPoint>] // This line is essential for a program as it defines the main entry point of this program.
let main argv =
  let result1 = removeOdd [1;2;3;4]
  let result2 = getSmallest [2;3;1;4]
  let result3 = take [ "a"; "b"; "c" ] 1u
  let result4 = runLength [ 1; 2; 2; 2; 3; 3 ]
  let result5 = isPalindrome [ 1; 2; 1 ]
  let result6 = slice [ 1; 2; 3; 4; 5 ] 2 4
  printfn "%A" result1
  printfn "%A" result2
  printfn "%A" result3
  printfn "%A" result4
  printfn "%A" result5
  printfn "%A" result6
  0 // DON't touch this; this is an integer exit code meaning successful termination.
