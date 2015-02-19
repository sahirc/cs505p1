module Expr (Var, Expr(..), CExpr(..), parseExpr, desugar, checkIds) where

import Data.List
import Result
import SExp

type Var = String

-- Expression syntax:
-- <e> ::= <number>
--       | "<string>"
--       | (if <e> <e> <e>)
--       | x
--       | (fun (x ...) <e>)
--       | (<e> <e> ...)
--       | (with* ([x <e>] ...) <e>)
data Expr = NumE Integer
          | StringE String
          | IfE Expr Expr Expr
          | VarE Var
          | FunE [Var] Expr
          | AppE [Expr]
          | WithStarE [(Var, Expr)] Expr
          deriving (Eq, Show)

-- Core language (desugared from Expr).
-- <e> ::= <number>
--       | "<string>"
--       | (if <e> <e> <e>)
--       | x
--       | (fun (x) <e>)
--       | (<e> <e>)
data CExpr = NumC Integer
           | StringC String
           | IfC CExpr CExpr CExpr
           | VarC Var
           | FunC Var CExpr
           | AppC CExpr CExpr
           deriving (Eq, Show)

parseExpr :: SExp -> Result Expr
parseExpr (NumS n) = return (NumE n)
parseExpr (IdS id) = return (VarE id)
parseExpr (StringS s) = return (StringE s)
parseExpr (ListS ((IdS "if"):sexps)) =
  case mapM parseExpr sexps of
   Ok [cond, cons, alt] -> return (IfE cond cons alt)
   _ -> fail ("bad `if` subexprs: " ++ (show sexps))
parseExpr (ListS [IdS "fun", ListS vars, body]) =
  do body' <- parseExpr body
     vars' <- mapM extractVar vars
     checkUnique vars'
     return (FunE vars' body')
parseExpr (ListS [IdS "with*", ListS bindings, body]) =
  do body' <- parseExpr body
     bindings' <- mapM parseBinding bindings
     return (WithStarE bindings' body')
parseExpr (ListS sexps) =
  do exprs <- mapM parseExpr sexps
     return (AppE exprs)

extractVar (IdS v) = return v
extractVar nonId = fail ("expected an id, got: " ++ (show nonId))

checkUnique vars | vars == (nub vars) = Ok ()
                 | otherwise = Err ("duplicate var in: " ++ (show vars))

parseBinding (ListS [IdS var, bound]) =
  do bound' <- parseExpr bound
     return (var, bound')
parseBinding bad = fail ("expected var-expr binding, got: " ++ (show bad))

desugar :: Expr -> Result CExpr
desugar (NumE n) = return (NumC n)
desugar (VarE v) = return (VarC v)
desugar (StringE s) = return (StringC s)
desugar (IfE cond cons alt) =
  do cond' <- desugar cond
     cons' <- desugar cons
     alt' <- desugar alt
     return (IfC cond' cons' alt')
desugar (FunE vars body) =
  case vars of
   [] -> fail "no-arg function"
   [var] ->
     do body' <- desugar body
        return (FunC var body')
   (var:vars) ->
     do fun' <- desugar (FunE vars body)
        return (FunC var fun')
desugar (WithStarE bindings body) =
  case bindings of
   [] -> desugar body
   ((var, expr):bindings') ->
     do expr' <- desugar expr
        body' <- desugar (WithStarE bindings' body)
        return (AppC (FunC var body') expr')
desugar (AppE exprs) =
  case exprs of
   [] -> fail "empty app"
   [expr] -> fail ("app with only one sub-expr: " ++ (show expr))
   [fun, arg] ->
     do fun' <- desugar fun
        arg' <- desugar arg
        return (AppC fun' arg')
   (fun:arg:args) -> desugar (AppE ((AppE [fun, arg]):args))

checkIds :: [String] -> [String] -> CExpr -> Result ()
checkIds bound reserved expr =
  let recur = checkIds bound reserved in
  case expr of
   NumC _ -> return ()
   StringC _ -> return ()
   IfC cond cons alt ->
     do recur cond
        recur cons
        recur alt
   VarC v | not (v `elem` bound) -> Err ("unbound id: " ++ v)
          | otherwise -> return ()
   FunC var body | var `elem` reserved -> Err ("cannot rebind reserved id: " ++ var)
                 | otherwise -> checkIds (var:bound) reserved body
   AppC fun arg ->
     do recur fun
        recur arg