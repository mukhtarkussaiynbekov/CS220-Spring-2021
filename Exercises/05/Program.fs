module CS220.Program // This line declares Program module.

open CS220.Library

// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

[<EntryPoint>] // This line is essential for a program as it defines the main entry point of this program.
let main argv =
  let result1 = countLetter "my name is Ann" "n"
  let result2 = diagonal (SqMatrix.init [ [1; 2; 3]; [4; 5; 6]; [7; 8; 9] ])
  let result3 = transpose (SqMatrix.init [ [1; 2; 3]; [4; 5; 6]; [7; 8; 9] ])
  let result4 = rotate [1;2;3;4] 1
  printfn "%A" result1
  printfn "%A" result2
  printfn "%A" result3
  printfn "%A" result4
  0 // DON't touch this; this is an integer exit code meaning successful termination.
