module Interp where

import SExp

-- Operations to support.
-- Concrete syntax:
-- <op> ::= + | * | = | <
data BinOp = Add | Mult | Equal | Lt deriving (Eq, Show)

-- Expressions.
-- Concrete syntax:
-- <e> ::= <number>
--       | true
--       | false
--       | (<op> <e> <e>)
--       | (if <e> <e> <e>)
data Expr = NumE Integer
          | BoolE Bool
          | BinOpE BinOp Expr Expr
          | IfE Expr Expr Expr
          deriving (Eq, Show)

parseExpr :: SExp -> Result Expr
parseExpr sexp = Err "unimplemented"

validParseExamples = [
  ("non-trivial example", "(if (= (* 2 3) (+ 5 1)) 7 10)",
   IfE (BinOpE Equal (BinOpE Mult (NumE 2) (NumE 3))
        (BinOpE Add (NumE 5) (NumE 1))) (NumE 7) (NumE 10))
  -- Feel free to add your own examples ...
  ]

checkValidParseExample (description, str, expected) =
  (description,
   case parseSExp (tokenize str) of
    Ok (sexp, []) -> parseExpr sexp == Ok expected
    _ -> False)

interp :: Expr -> Result Expr
interp expr = Err "unimplemented"
