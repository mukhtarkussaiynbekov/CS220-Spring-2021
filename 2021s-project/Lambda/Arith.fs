module Lambda.Arith

/// Please use the builder to construct expressions.
open DeBruijnBuilder

/// True (ル ル 2).
let t: DeBruijnExpr = failwith "IMPLEMENT"

/// False (ル ル 1).
let f: DeBruijnExpr = failwith "IMPLEMENT"

/// If-then-else. (ル ル ル ((3 2) 1))
let ite: DeBruijnExpr = failwith "IMPLEMENT"

/// Pair. (ル ル ル ((1 3) 2))
let pair: DeBruijnExpr = failwith "IMPLEMENT"

/// Car. (ル (1 ル ル 2))
let fst: DeBruijnExpr = failwith "IMPLEMENT"

/// Cdr. (ル (1 ル ル 1))
let snd: DeBruijnExpr = failwith "IMPLEMENT"

/// Zero (ル ル 1).
let zero = f (* Zero = False *)

/// Successor (ル ル ル (2 ((3 2) 1))).
let succ: DeBruijnExpr = failwith "IMPLEMENT"

/// IsZero?
let isZero: DeBruijnExpr = failwith "IMPLEMENT"

/// Church numeral: One.
let one: DeBruijnExpr = failwith "IMPLEMENT"

/// Church numeral: Two.
let two: DeBruijnExpr = failwith "IMPLEMENT"

/// Church numeral: Three.
let three: DeBruijnExpr = failwith "IMPLEMENT"

/// Church numeral: Four.
let four: DeBruijnExpr = failwith "IMPLEMENT"

/// Church numeral: Five.
let five: DeBruijnExpr = failwith "IMPLEMENT"

/// Church numeral: Six.
let six: DeBruijnExpr = failwith "IMPLEMENT"

/// Church numeral: Seven.
let seven: DeBruijnExpr = failwith "IMPLEMENT"

/// Church numeral: Eight.
let eight: DeBruijnExpr = failwith "IMPLEMENT"

/// Church numeral: Nine.
let nine: DeBruijnExpr = failwith "IMPLEMENT"

/// Addition.
let add: DeBruijnExpr = failwith "IMPLEMENT"

/// Predecessor.
let pred: DeBruijnExpr = failwith "IMPLEMENT"

/// Subtraction.
let sub: DeBruijnExpr = failwith "IMPLEMENT"

/// Convert a Church number to a natural number.
/// val toNatural: DeBruijnExpr -> int
let toNatural (dexpr: DeBruijnExpr) = failwith "IMPLEMENT"

/// Multiplication.
let mul: DeBruijnExpr = failwith "IMPLEMENT"

/// Factorial.
let factorial: DeBruijnExpr = failwith "IMPLEMENT"
