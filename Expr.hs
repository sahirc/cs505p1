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
sexpToVar :: [SExp] -> [Var]
sexpToVar [] = []
sexpToVar (v:vs) = case v of IdS var -> [var] ++ sexpToVar(vs)													  

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
							else if tok == "fun"
							  then case (t !! 1) of
							  	ListS varExprs -> Ok(FunE (sexpToVar(varExprs)) tok2)												
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
desugar expr = 
  case expr of 
  	NumE i -> Ok(NumC i)
  	AppE [a,b] -> Ok(AppC x y)
  				  	where
  					  x = (case (desugar(a)) of Ok(x') -> x'::CExpr)::CExpr
  					  y = (case (desugar(b)) of Ok(x') -> x'::CExpr)::CExpr
  	AppE (a:b:exprs) -> desugar(AppE ((AppE [a,b]) : exprs))

  	IfE cond cons alt -> Ok(IfC (cond') (cons') (alt'))
  						   where cond' = case (desugar(cond)) of Ok(x) -> x
  						     	 cons' = case (desugar(cons)) of Ok(x) -> x
  						     	 alt'  = case (desugar(alt))  of Ok(x) -> x

  	VarE v -> Ok(VarC v)
  	FunE (v:vs) expr -> Ok(FunC v (parseFun (vs) (expr)))
  	WithStarE [] expr -> desugar(expr)
  	WithStarE a@(bg:bgs) expr -> Ok(AppC (FunC (fst(bg)) cbg) rbg)
  							     where rbg = case (desugar(snd(bg))) of Ok(x) -> x
  							           cbg = case (desugar( WithStarE bgs expr )) of Ok(x) -> x

parseFun :: [Var] -> Expr -> CExpr
parseFun vars@(v:vs) exprs = if (length(vars) == 1) 
							   then (FunC v e)
							 else if (length(vars) >= 2)
							  	then FunC v (parseFun (vs) (exprs))
							 else
							  	error "ERROR"
							 where e = case desugar(exprs) of Ok(cexpr) -> cexpr 

checkIds :: [String] -> [String] -> CExpr -> Result ()
checkIds bound reserved expr = 
			case expr of 
				VarC v -> if (v `elem` reserved) 
							then Err(show(v) ++ "is a reserved var")
						  else 
							Ok()
				IfC cond cons alt -> Err (show(cond) ++ show(cons) ++ show(alt))