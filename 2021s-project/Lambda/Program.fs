module Lambda.Program

open System

let rec parseLoop (parser: Parse.ExprParser) =
  Console.Write ("> ")
  let line = Console.ReadLine ()
  if line.StartsWith "q" then ()
  elif String.IsNullOrEmpty line then parseLoop parser
  else
    match parser.Run line with
    | Ok (expr, dexpr) ->
      expr |> Expr.toString |> Console.WriteLine
      dexpr |> DeBruijnExpr.toString |> Console.WriteLine
      (Evaluate.nf dexpr) |> DeBruijnExpr.toString |> Console.WriteLine
    | Error (msg) -> Console.WriteLine ("Failed to parse: {0}", msg)
    parseLoop parser

[<EntryPoint>]
let main _argv =
  Console.OutputEncoding <- Text.Encoding.Unicode // Just to make sure.
  parseLoop (Parse.ExprParser ())
  0