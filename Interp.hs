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
parseExpr sexp =
  case sexp of
    NumS i -> Ok(NumE i)
    IdS s -> if s == "true"
               then Ok(BoolE True)
             else 
               Ok(BoolE False)       
    ListS t -> case (head (t)) of
                 IdS tok -> if tok == "+"
                              then Ok(BinOpE Add tok1 tok2)
                            else if tok == "*"
                              then Ok(BinOpE Mult tok1 tok2)
                            else if tok == "="
                              then Ok(BinOpE Equal tok1 tok2)
                            else if tok == "<"
                              then Ok(BinOpE Lt tok1 tok2)
                            else if tok == "if"
                              then Ok(IfE tok1 tok2 tok3)
                            else 
                              Err "Unknown SExp"                              
                            where tok1 = case (parseExpr (t !! 1)) of
                                           Ok(exp) -> exp 
                                  tok2 = case (parseExpr (t !! 2)) of
                                           Ok(exp) -> exp 
                                  tok3 = case (parseExpr (t !! 3)) of
                                           Ok(exp) -> exp 


validParseExamples = [
  ("non-trivial example", "(if (= (* 2 3) (+ 5 1)) 7 10)",
   IfE (BinOpE Equal (BinOpE Mult (NumE 2) (NumE 3))
        (BinOpE Add (NumE 5) (NumE 1))) (NumE 7) (NumE 10))
  -- Feel free to add your own examples ...
  ]

validInterpExamples = [
  ("Single Mult", "(* 2 3)", NumE 6),
  ("Single Add", "(+ 2 3)", NumE 5),
  ("Single equal", "(= 2 3)", BoolE False),
  ("Single <", "(< 2 3)", BoolE True),
  ("non-trivial equal", "(if (= (* 2 3) (+ 5 1)) 7 10)", NumE 7),
  ("non-trivial <", "    (if (< (* 2 3) (+ 15 1)) 7 10)", NumE 7),
  ("nested non-trivial <", "    (if (< (* 5 3) (+ (+ 5 1) (+ 5 1) )) 7 10)", NumE 10)    
  ]  

checkValidParseExample (description, str, expected) =
  (description,
   case parseSExp (tokenize str) of
    Ok (sexp, []) -> parseExpr sexp == Ok expected
    _ -> False)

checkValidInterpExample (description, str, expected) =
  (description,
   case parseSExp (tokenize str) of
    Ok (sexp, []) -> case (parseExpr(sexp)) of
      Ok(expr) -> interp (expr) == Ok expected
    _ -> False)  

interp :: Expr -> Result Expr
interp expr = 
  case expr of
    NumE number -> Ok(NumE number) 
    BoolE bool -> Ok(BoolE bool)
    BinOpE op lhs rhs | op == Add -> Ok(NumE (ilhs + irhs))
                      | op == Mult -> Ok(NumE (ilhs * irhs))
                      | op == Equal -> Ok(BoolE (ilhs == irhs))
                      | op == Lt -> Ok(BoolE (ilhs < irhs))
      where ilhs = case interp(lhs) of 
              Ok(expr') -> case expr' of
                NumE number -> number
            irhs = case interp(rhs) of 
              Ok(expr') -> case expr' of
                NumE number -> number
    IfE cond cons alt -> case interp(cond) of 
                          Ok(expr') -> case expr' of
                            BoolE bool | bool == True -> interp(cons)
                                       | otherwise -> interp(alt)