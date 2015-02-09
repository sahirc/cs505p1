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
parseExpr (NumS i) = return (NumE i)
parseExpr (IdS i) = return (VarE i)
parseExpr (ListS [IdS "if", cond, cons, alt]) = parseExpr cond >>= (\cond' ->
											      parseExpr cons >>= (\cons' ->
											  	    parseExpr alt >>= (\alt' ->
											  	      return (IfE cond' cons' alt'))))

parseExpr (ListS [IdS "with*", ListS varExprs, sexp]) =  parseExpr sexp >>= (\expr -> 
														   return (WithStarE (sexpToVarExpr varExprs) expr))
parseExpr (ListS [IdS "fun", ListS varExprs, sexp]) =  parseExpr sexp >>= (\expr -> 
													     return (FunE (sexpToVar varExprs) expr))
parseExpr (ListS [ListS s]) = parseExpr (ListS s)
parseExpr (ListS list) = return(AppE (sexpToExpr list))

-- Core language (desugar'ed from Expr).
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
 
boundVars = ["+","*","<","=","if","true","false", "box", "set-box!", "unbox"]
reservedVars = ["if","true","false"]

desugar :: Expr -> Result CExpr
desugar expr = desugar' expr >>= (\cexpr -> checkIds boundVars reservedVars cexpr >>= (\_ -> return cexpr))

desugar' :: Expr -> Result CExpr
-- Vars are just vars
desugar' (VarE v) = return(VarC v)
-- Ints are just ints
desugar' (NumE i) = return(NumC i)
-- If nothing is bound, all you can do is desugar' the expr
desugar' (WithStarE [] expr) = desugar' expr
-- Simplest AppE case to convert to AppC
desugar' (AppE [a,b]) = desugar' a >>= (\a' -> desugar' b >>= (\b' -> return (AppC a' b')))

-- Break down nested AppEs into the simplest case mentioned above  					  		
desugar' (AppE (a:b:exprs)) = desugar' (AppE ((AppE [a,b]) : exprs))  					  		
desugar' (IfE cond cons alt) = desugar' cond >>= (\cond' ->
								desugar' cons >>= (\cons' ->
								  desugar' alt >>= (\alt' -> 
								  	return (IfC cond' cons' alt') 
								  )
								)
							  )
-- If the fun has a single param, no need to curry																
desugar'  (FunE [v] expr) = desugar' expr >>= (\dexpr -> return (FunC v dexpr))
-- Multi-param functions must be curried  							
desugar' (FunE (v:vs) expr) = parseFun vs expr >>= (\remFun -> return (FunC v remFun))

desugar' (WithStarE (bg:bgs) expr) = desugar' (snd bg) >>= (\rbg -> 
									  desugar' (WithStarE bgs expr) >>= (\cbg -> 
									  	return (AppC (FunC (fst(bg)) cbg) rbg)))

parseFun :: [Var] -> Expr -> Result CExpr
parseFun vars@(v:vs) exprs = if (length(vars) == 1) 
							   then Ok(FunC v e)
							 else if (length(vars) >= 2)
							  	then case parseFun vs exprs of
							  		Ok(remFun) -> Ok(FunC v remFun)
							  		Err(msg) -> Err(msg)
							 else
							  	Err "functions must have parameters"
							 where e = case desugar (exprs) of 
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