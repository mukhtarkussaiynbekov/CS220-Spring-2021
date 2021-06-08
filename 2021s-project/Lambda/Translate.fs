module Lambda.Translate

exception UnknownIdentifierException of string

/// val toDebrijn: Expr -> DeBruijnExpr
let toDebruijn (expr: Expr) = failwith "IMPLEMENT"

/// val toExpr: DeBruijnExpr -> Expr
let toExpr (dexpr: DeBruijnExpr) = failwith "IMPLEMENT"