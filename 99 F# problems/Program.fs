module CS220.Program // This line declares Program module.

open CS220.Lists

// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

[<EntryPoint>] // This line is essential for a program as it defines the main entry point of this program.
let main argv =
  let result1 = myLast [1;2;3;4]
  let result2 = myButLast [1;2;3;4]
  let result3 = elementAt [1; 2; 3] 2
  let result4 = myLength <| List.ofSeq "Hello, world!"
  let result5 = reverse [1;2;3;4]
  let result6 = isPalindrome <| List.ofSeq "madamimadam"
  let result7 = flatten (List [Elem 1; List [Elem 2; List [Elem 3; Elem 4]; Elem 5]])
  let result8 = compress ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"]
  let result9 = pack ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"]
  let result10 = encode ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"]
  let result11 = encodeModified <| List.ofSeq "aaaabccaadeeee"
  let result12 = decodeModified [Multiple (4,'a'); Single 'b'; Multiple (2,'c'); Multiple (2,'a'); Single 'd'; Multiple (4,'e')]
  let result13 = encodeDirect <| List.ofSeq "aaaabccaadeeee"
  let result14 = dupli [1; 2; 3]
  let result15 = repli (List.ofSeq "abc") 3
  let result16 = dropEvery (List.ofSeq "abcdefghik") 3
  let result17 = split (List.ofSeq "abcdefghik") 3
  let result18 = slice ['a';'b';'c';'d';'e';'f';'g';'h';'i';'k'] 3 7
  let result19 = rotate ['a';'b';'c';'d';'e';'f';'g';'h'] (-2)
  let result20 = removeAt 1 <| List.ofSeq "abcd"
  printfn "%A" result1
  printfn "%A" result2
  printfn "%A" result3
  printfn "%A" result4
  printfn "%A" result5
  printfn "%A" result6
  printfn "%A" result7
  printfn "%A" result8
  printfn "%A" result9
  printfn "%A" result10
  printfn "%A" result11
  printfn "%A" result12
  printfn "%A" result13
  printfn "%A" result14
  printfn "%A" result15
  printfn "%A" result16
  printfn "%A" result17
  printfn "%A" result18
  printfn "%A" result19
  printfn "%A" result20
  0 // DON't touch this; this is an integer exit code meaning successful termination.
