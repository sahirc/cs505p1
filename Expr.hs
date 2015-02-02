module Expr (Var, Expr(..), CExpr(..), parseExpr, desugar, checkIds) where

import Data.List
import Result
import SExp

type Var = String

-- Expression syntax:
-- <e> ::= <number>
--       | (if <e> <e> <e>)  
--       | x
--       | (fun (x ...) <e>)
--       | (<e> <e> ...)
--       | (with* ([x <e>] ...) <e>)
data Expr = NumE Integer
          | IfE Expr Expr Expr
          | VarE Var
          | FunE [Var] Expr
          | AppE [Expr]
          | WithStarE [(Var, Expr)] Expr
          deriving (Eq, Show)
		  
withStarListHelper :: [SExp] -> [(Var, Expr)] 	
withStarListHelper [] = []			  
withStarListHelper (v:vs) = case v of								
								ListS list -> [(var, expr)] ++ withStarListHelper(vs)
												where 
													var = case (parseExpr(list !! 0)) of 
														Ok(exp) -> case exp of
															VarE var' -> var'
													expr = case (parseExpr(list !! 1)) of
														Ok(exp) -> exp														  
parseExpr :: SExp -> Result Expr
parseExpr sexp =
  case sexp of
    NumS i -> Ok(NumE i)
    IdS s -> Ok(VarE s)
    ListS t -> case (head (t)) of
                IdS tok -> if tok == "+"
                              then Ok(AppE [VarE "+", tok1, tok2])
                            else if tok == "*"
                              then Ok(AppE [VarE "*", tok1, tok2])
                            else if tok == "="
                              then Ok(AppE [VarE "=", tok1, tok2])
                            else if tok == "<"
                              then Ok(AppE [VarE "<", tok1, tok2])
                            else if tok == "if"
                              then Ok(IfE tok1 tok2 tok3)
							else if tok == "with*"
							  then case (t !! 1) of 
								ListS varExprs -> Ok(WithStarE (withStarListHelper(varExprs)) tok2)													
                            else
                              Err "Unknown SExp"
                            where tok1 = case (parseExpr (t !! 1)) of
                                           Ok(exp) -> exp
                                  tok2 = case (parseExpr (t !! 2)) of
                                           Ok(exp) -> exp
                                  tok3 = case (parseExpr (t !! 3)) of
                                           Ok(exp) -> exp
-- Core language (desugared from Expr).
-- <e> ::= <number>
--       | (if <e> <e> <e>)
--       | x
--       | (fun (x) <e>)
--       | (<e> <e>)
data CExpr = NumC Integer
           | IfC CExpr CExpr CExpr
           | VarC Var
           | FunC Var CExpr
           | AppC CExpr CExpr
           deriving (Eq, Show)
 
desugar :: Expr -> Result CExpr
desugar expr = Err "desugar not implemented yet"

checkIds :: [String] -> [String] -> CExpr -> Result ()
checkIds bound reserved expr = 
			case expr of 
				VarC v -> if (v `elem` reserved) 
							then Err(show(v) ++ "is a reserved var")
						  else 
							Ok()
				IfC cond cons alt -> Err (show(cond) ++ show(cons) ++ show(alt))