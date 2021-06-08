namespace Lambda.Tests

open Lambda
open Lambda.DeBruijnBuilder
open Microsoft.VisualStudio.TestTools.UnitTesting

[<TestClass>]
type TestClass () =

  let parser = Parse.ExprParser ()

  [<TestMethod>]
  member _.``Basic Expr Parsing Test`` () =
    match "(\x. (\y. (x y)))" |> parser.Run with
    | Ok (expr, dexpr) ->
      let expectedExpr = Lambda ("x", Lambda ("y", Apply (Var "x", Var "y")))
      let expectedDExpr = abs (abs (ref 2 $ ref 1))
      Assert.AreEqual (expectedExpr, expr)
      Assert.AreEqual (expectedDExpr, dexpr)
    | Error msg -> Assert.Fail (msg)

  [<TestMethod>]
  member _.``Value Test (true)`` () =
    let e = Arith.t
    let nf = e |> Evaluate.nf
    Assert.AreEqual (e, nf)

  [<TestMethod>]
  member _.``Value Test (false)`` () =
    let e = Arith.f
    let nf = e |> Evaluate.nf
    Assert.AreEqual (e, nf)

  [<TestMethod>]
  member _.``Value Test (ite)`` () =
    let e = Arith.ite
    let nf = e |> Evaluate.nf
    Assert.AreEqual (e, nf)

  [<TestMethod>]
  member _.``Value Test (ite 2)`` () =
    let res = Arith.ite $ Arith.t $ Arith.f $ Arith.t |> Evaluate.nf
    Assert.AreEqual (Arith.f, res)
    let res = Arith.ite $ Arith.f $ Arith.f $ Arith.t |> Evaluate.nf
    Assert.AreEqual (Arith.t, res)

  [<TestMethod>]
  member _.``Value Test (pair 1)`` () =
    let e = Arith.pair
    let nf = e |> Evaluate.nf
    Assert.AreEqual (e, nf)
    let e = Arith.fst
    let nf = e |> Evaluate.nf
    Assert.AreEqual (e, nf)
    let e = Arith.snd
    let nf = e |> Evaluate.nf
    Assert.AreEqual (e, nf)

  [<TestMethod>]
  member _.``Value Test (pair 2)`` () =
    let x = Arith.pair $ Arith.t $ Arith.f
    let res1 = Arith.fst $ x |> Evaluate.nf
    let res2 = Arith.snd $ x |> Evaluate.nf
    Assert.AreEqual (Arith.t, res1)
    Assert.AreEqual (Arith.f, res2)

  [<TestMethod>]
  member _.``Value Test (basic arith)`` () =
    let e = Arith.zero
    let nf = e |> Evaluate.nf
    Assert.AreEqual (e, nf)
    let e = Arith.succ
    let nf = e |> Evaluate.nf
    Assert.AreEqual (e, nf)
    let e = Arith.isZero
    let nf = e |> Evaluate.nf
    Assert.AreEqual (e, nf)

  [<TestMethod>]
  member _.``Value Test (isZero)`` () =
    let e = Arith.isZero $ Arith.zero |> Evaluate.nf
    Assert.AreEqual (Arith.t, e)
    let e = Arith.isZero $ (Arith.succ $ Arith.zero) |> Evaluate.nf
    Assert.AreEqual (Arith.f, e)

  [<TestMethod>]
  member _.``Value Test (Church numerals 1)`` () =
    let e = Arith.succ $ Arith.zero |> Evaluate.nf
    Assert.AreEqual (Arith.one, e)
    let e = Arith.succ $ Arith.one |> Evaluate.nf
    Assert.AreEqual (Arith.two, e)
    let e = Arith.succ $ Arith.two |> Evaluate.nf
    Assert.AreEqual (Arith.three, e)
    let e = Arith.succ $ Arith.three |> Evaluate.nf
    Assert.AreEqual (Arith.four, e)
    let e = Arith.succ $ Arith.four |> Evaluate.nf
    Assert.AreEqual (Arith.five, e)

  [<TestMethod>]
  member _.``Value Test (Church numerals 2)`` () =
    let e = Arith.ite $ Arith.t $ Arith.four $ Arith.two |> Evaluate.nf
    Assert.AreEqual (Arith.four, e)
    let e = Arith.ite $ Arith.f $ Arith.four $ Arith.two |> Evaluate.nf
    Assert.AreEqual (Arith.two, e)

  [<TestMethod>]
  member _.``Value Test (Church numerals addition)`` () =
    let e = Arith.add
    let nf = e |> Evaluate.nf
    Assert.AreEqual (e, nf)
    let e = Arith.add $ Arith.two $ Arith.three |> Evaluate.nf
    Assert.AreEqual (Arith.five, e)
    let e = Arith.add $ Arith.two $ Arith.zero |> Evaluate.nf
    Assert.AreEqual (Arith.two, e)

  [<TestMethod>]
  member _.``Value Test (Church numerals pred)`` () =
    let e = Arith.pred
    let nf = e |> Evaluate.nf
    Assert.AreEqual (e, nf)
    let e = Arith.pred $ Arith.four |> Evaluate.nf
    Assert.AreEqual (Arith.three, e)
    let e1 = Arith.pred $ Arith.six |> Evaluate.nf
    let e2 = Arith.succ $ Arith.four |> Evaluate.nf
    Assert.AreEqual (e1, e2)

  [<TestMethod>]
  member _.``Value Test (Church numerals subtraction)`` () =
    let e = Arith.sub
    let nf = e |> Evaluate.nf
    Assert.AreEqual (e, nf)
    let e = Arith.sub $ Arith.eight $ Arith.three |> Evaluate.nf
    Assert.AreEqual (Arith.five, e)
    let e = Arith.sub $ Arith.seven $ Arith.seven |> Evaluate.nf
    Assert.AreEqual (Arith.zero, e)

  [<TestMethod>]
  member _.``Value Test (Church numerals to natural number)`` () =
    let n = Arith.toNatural Arith.three
    Assert.AreEqual (3, n)
    let n = Arith.toNatural Arith.zero
    Assert.AreEqual (0, n)
    let n = Arith.add $ Arith.nine $ Arith.eight |> Evaluate.nf
    let n = Arith.toNatural n
    Assert.AreEqual (17, n)

  [<TestMethod>]
  member _.``Value Test (Church numerals multiplication)`` () =
    let e = Arith.mul
    let nf = e |> Evaluate.nf
    Assert.AreEqual (e, nf)
    let e = Arith.mul $ Arith.six $ Arith.seven |> Evaluate.nf
    let n = Arith.toNatural e
    Assert.AreEqual (42, n)
    let e = Arith.mul $ Arith.zero $ Arith.eight |> Evaluate.nf
    let n = Arith.toNatural e
    Assert.AreEqual (0, n)
    let e = Arith.mul $ Arith.eight $ Arith.zero |> Evaluate.nf
    let n = Arith.toNatural e
    Assert.AreEqual (0, n)

  [<TestMethod>]
  member _.``Value Test (factorial)`` () =
    let v = Arith.factorial $ Arith.five |> Evaluate.nf |> Arith.toNatural
    Assert.AreEqual (120, v)
