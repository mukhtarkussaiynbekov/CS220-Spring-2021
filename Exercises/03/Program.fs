module CS220.Program // This line declares Program module.

open CS220.Library

// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

[<EntryPoint>] // This line is essential for a program as it defines the main entry point of this program.
let main argv =
  let result2 = pair 1 2 |> fst
  let one = {Hours = 1; Minutes = 45; AMPM = AM}
  let two = {Hours = 2; Minutes = 45; AMPM = AM}
  let result3 = isEarly one two
  let result4 = (abs -11) % 5
  printfn "%A" result2
  printfn "%A" result3
  printfn "%A" result4
  0 // DON't touch this; this is an integer exit code meaning successful termination.
