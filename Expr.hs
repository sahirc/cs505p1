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
		  
sexpToVarExpr :: [SExp] -> [(Var, Expr)] 	
sexpToVarExpr [] = []			  
sexpToVarExpr (v:vs) = case v of								
							  ListS list -> [(var, expr)] ++ sexpToVarExpr(vs)
						  	    where 
								  var = case (parseExpr(list !! 0)) of 
								    Ok(exp) -> case exp of 
									  VarE var' -> var'
								  expr = case (parseExpr(list !! 1)) of
									Ok(exp) -> exp	
sexpToVar :: [SExp] -> [Var]
sexpToVar [] = []
sexpToVar (v:vs) = case v of IdS var -> [var] ++ sexpToVar(vs)													  

sexpToExpr :: [SExp] -> [Expr]
sexpToExpr [] = []
sexpToExpr (s:ss) = case (parseExpr s) of Ok(x) -> [x] ++ sexpToExpr(ss)											  

parseExpr :: SExp -> Result Expr
parseExpr sexp =
  case sexp of
    NumS i -> Ok(NumE i)
    IdS s -> Ok(VarE s)
    ListS t -> case (head (t)) of
                IdS tok -> if tok == "if"
                              then Ok(IfE tok1 tok2 tok3)
							else if tok == "with*"
							  then case (t !! 1) of 
								ListS varExprs -> Ok(WithStarE (sexpToVarExpr(varExprs)) tok2)
								_ -> Err "First element of with wasn't a list of bindings"
							else if tok == "fun"
							  then case (t !! 1) of
							  	ListS varExprs -> Ok(FunE (sexpToVar(varExprs)) tok2)
							  	_ -> Err "First element of fun wasn't a list of vars"												
                            else
                              Ok(AppE (sexpToExpr t)) 
                            where tok1 = case (parseExpr (t !! 1)) of
                                           Ok(exp) -> exp
                                  tok2 = case (parseExpr (t !! 2)) of
                                           Ok(exp) -> exp
                                  tok3 = case (parseExpr (t !! 3)) of
                                           Ok(exp) -> exp       
                ListS s -> parseExpr (ListS s)
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
-- Vars are just vars
desugar (VarE v) = Ok(VarC v)
-- Ints are just ints
desugar (NumE i) = Ok(NumC i)
-- If nothing is bound, all you can do is desugar the expr
desugar (WithStarE [] expr) = desugar(expr)
-- Simplest AppE case to convert to AppC
desugar (AppE [a,b]) = case desugar a of 
						Ok(a') -> case desugar b of
							Ok(b') -> Ok(AppC a' b')
							Err(x) -> Err(x)
						Err(x) -> Err(x)
-- Break down nested AppEs into the simplest case mentioned above  					  		
desugar (AppE (a:b:exprs)) = desugar(AppE ((AppE [a,b]) : exprs))  					  		
desugar (IfE cond cons alt) = case desugar cond of
								Ok(cond') -> case desugar cons of 
									Ok(cons') -> case desugar alt of 
										Ok(alt') -> Ok(IfC cond' cons' alt')
										Err(x) -> Err(x)
									Err(x) -> Err(x)
								Err(x) -> Err(x)
-- If the fun has a single param, no need to curry																
desugar  (FunE [v] expr) = case desugar expr of 
  							Ok(dexpr) -> Ok(FunC v dexpr)
  							Err(x) -> Err(x)	
-- Multi-param functions must be curried  							
desugar (FunE (v:vs) expr) = Ok(FunC v (parseFun vs expr))
desugar (WithStarE (bg:bgs) expr) = case desugar(snd bg) of
  								      Ok(rbg) -> case desugar (WithStarE bgs expr) of
  								   	    Ok(cbg) -> Ok(AppC (FunC (fst(bg)) cbg) rbg)
  								   	    Err(x) -> Err(x)
  								      Err(x) -> Err(x)  							     

parseFun :: [Var] -> Expr -> CExpr
parseFun vars@(v:vs) exprs = if (length(vars) == 1) 
							   then (FunC v e)
							 else if (length(vars) >= 2)
							  	then FunC v (parseFun vs exprs)
							 else
							  	error "ERROR"
							 where e = case desugar(exprs) of 
							 	Ok(cexpr) -> cexpr 
							 	Err(msg) -> error (show(msg))
							 	

checkIds :: [String] -> [String] -> CExpr -> Result ()
checkIds bound reserved expr = case expr of 
							     AppC c1 c2 -> case checkIds bound reserved c1 of
								 			     Ok(_) -> case checkIds bound reserved c2 of 
								 			     	Ok(_) -> Ok()
								 			     	Err(x) -> Err(x)
								 			     Err(x) -> Err(x)
							     VarC v -> if (v `elem` reserved) 
							                 then Err(show(v) ++ "is a reserved var in " ++ show(reserved))
						  				   else if (v `elem` bound)
						  				     then
						  				       Ok()
						  			       else  
									         Err(show(v) ++ "is unbound var in " ++ show(bound))
							     FunC v c -> checkIds (v:bound) reserved c
							     IfC cond cons alt -> case checkIds bound reserved cond of
							     						Ok(_) -> case checkIds bound reserved cons of
							     							Ok(_) -> case checkIds bound reserved alt of
							     							  Ok(_) -> Ok()
							     							  Err(x) -> Err(x)
							     							Err(x) -> Err(x)
							     						Err(x) -> Err(x)
							     NumC _ -> Ok()

checkIdTestCases = [
  ("multiple bindings", "+":"*":[], "if":"true":[], "(with* ([x (+ 1 2)] [y (* x x)] [x (+ y 3)]) (+ x y))", Ok()),
  ("unbound z", "+":"*":[], "if":"true":[], "(with* ([x (+ 1 2)] [y (* x x)] [x (+ y 3)]) (+ z y))", Err("some error")),
  ("bound reserved word if", "+":"*":[], "if":"true":[], "(with* ([if (+ 1 2)] [y (* x x)] [x (+ y 3)]) (+ x y))", Err("some error")),
  ("bound reserved word in if ", "+":"*":[], "if":"true":[], "(if (< (* x 3) (+ (+ 5 1) (+ 5 1) )) 7 10)", Err("some error")),
  ("unbound in body",  "+":"*":[], "if":"true":[], "(fun (x) (+ 2 y))", Err("some error"))	
  ]  		

testCheckId (description, bound, reserved, str, expected) =
  (description,
   case parseSExp (tokenize str) of
    Ok (sexp, []) -> case parseExpr sexp of
      Ok(expr) -> case desugar expr of 
        Ok(dexpr) -> case checkIds bound reserved dexpr of
        	Ok(_) -> expected == Ok()
        	Err(_) -> expected == Err("some error")

  )			     							  