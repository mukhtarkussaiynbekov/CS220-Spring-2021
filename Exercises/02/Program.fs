module CS220.Program // This line declares Program module.

open CS220.Library

// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

[<EntryPoint>] // This line is essential for a program as it defines the main entry point of this program.
let main argv =
  let result1 = prob1 2
  let result4 = prob4 4
  let result5 = prob5 2 3
  let result6 = prob6 0 35
  let result7 = pow "abc" 3
  let result8 = smallestDivisor 45u
  let result9 = isPrime 3u
  printfn "%f" result1
  printfn "%d" result4
  printfn "%d" result5
  printfn "%d" result6
  printfn "%s" result7
  printfn "%d" result8
  printfn "%b" result9
  0 // DON't touch this; this is an integer exit code meaning successful termination.
