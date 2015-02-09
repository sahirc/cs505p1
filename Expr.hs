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
		  
sexpToVarExpr :: [SExp] -> Result [(Var, Expr)] 	
sexpToVarExpr [] = return []			  
sexpToVarExpr (v:vs) = case v of ListS (a:b:_) -> parseExpr a >>= (\a' -> 
														parseExpr b >>= (\b' ->
															sexpToVarExpr vs >>= (\vs' ->
																case a' of 
																	VarE var -> return ((var, b') : vs')
																	nonVar -> fail ("I was expcting a var " ++ show(nonVar))
																)))
															

sexpToVar :: [SExp] -> Result [Var]
sexpToVar [] = Ok []
sexpToVar (v:vs) = case v of 
					IdS var -> sexpToVar vs >>= (\vs' -> return (var : vs'))
					nonId -> fail ("I was expcting a var " ++ show(nonId))

sexpToExpr :: [SExp] -> Result [Expr]
sexpToExpr [] = Ok []
sexpToExpr (s:ss) = parseExpr s >>= (\s' -> 
						sexpToExpr ss >>= (\ss' -> 
							return (s' : ss') 
					    )
					)

parseExpr :: SExp -> Result Expr
parseExpr (NumS i) = return (NumE i)
parseExpr (IdS i) = return (VarE i)
parseExpr (ListS [IdS "if", cond, cons, alt]) = parseExpr cond >>= (\cond' ->
											      parseExpr cons >>= (\cons' ->
											  	    parseExpr alt >>= (\alt' ->
											  	      return (IfE cond' cons' alt'))))

parseExpr (ListS [IdS "with*", ListS [], _]) =  fail "with* format not met"
parseExpr (ListS [IdS "with*", ListS varExprs, sexp]) =  parseExpr sexp >>= (\expr -> sexpToVarExpr varExprs >>= (\v' -> return (WithStarE v' expr)))

parseExpr (ListS [IdS "fun", ListS [], _]) =  fail "functions must have parameters"
parseExpr (ListS [IdS "fun", ListS varExprs, sexp]) = parseExpr sexp >>= (\expr -> sexpToVar varExprs >>= (\v' -> return (FunE v' expr)))

parseExpr (ListS [ListS s]) = parseExpr (ListS s)
parseExpr (ListS list) = sexpToExpr list >>= (\list' -> return (AppE list'))

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
 
boundVars = ["+","*","<","=","if","true","false", "box", "set-box!", "unbox"]
reservedVars = ["if","true","false"]

desugar :: Expr -> Result CExpr
-- Vars are just vars
desugar (VarE v) = return(VarC v)
-- Ints are just ints
desugar (NumE i) = return(NumC i)
-- If nothing is bound, all you can do is desugar the expr
desugar (WithStarE [] expr) = desugar expr
desugar (WithStarE ((firstVarName, firstBinding):bgs) expr) = desugar firstBinding >>= (\rbg -> 
									  desugar (WithStarE bgs expr) >>= (\cbg -> 
									  	return (AppC (FunC firstVarName cbg) rbg)))
-- Simplest AppE case to convert to AppC
desugar (AppE [a,b]) = desugar a >>= (\a' -> desugar b >>= (\b' -> return (AppC a' b')))
-- Break down nested AppEs into the simplest case mentioned above  					  		

desugar (AppE (a:b:exprs)) = desugar (AppE ((AppE [a,b]) : exprs))  					  		
desugar (IfE cond cons alt) = desugar cond >>= (\cond' ->
								desugar cons >>= (\cons' ->
								  desugar alt >>= (\alt' -> 
								  	return (IfC cond' cons' alt') 
								  )
								)
							  )
desugar (FunE [] expr) = fail "functions must have at least one parameter"
-- If the fun has a single param, no need to curry																
desugar  (FunE [v] expr) = desugar expr >>= (\dexpr -> return (FunC v dexpr))
-- Multi-param functions must be curried  							
desugar (FunE (v:vs) expr) = desugar (FunE [v] (FunE vs expr))

desugar _ = fail "unrecognized expression"

checkIds :: [String] -> [String] -> CExpr -> Result ()
checkIds bound reserved expr = case expr of 
							     AppC c1 c2 -> checkIds bound reserved c1 >>= (\c1' -> 
								 			     checkIds bound reserved c2 >>= (\_ -> return ()))
							     VarC v -> if (v `elem` reserved) 
							                 then fail(show(v) ++ " is a reserved var in " ++ show(reserved))
						  				   else if (v `elem` bound)
						  				     then return () 
						  			       else  
									         fail(show(v) ++ " is an unbound var in " ++ show(bound))
							     FunC v c -> checkIds (v:bound) reserved (VarC v) >>= (\_ -> checkIds (v:bound) reserved c)
							     IfC cond cons alt -> checkIds bound reserved cond >>= (\cond' ->
							     						checkIds bound reserved cons >>= (\cons' ->
							     						  checkIds bound reserved alt >>= (\alt' ->
							     						  	return ()
							     						  ))) 
							     NumC _ -> return ()

checkIdTestCases = [
  ("something", "+":"*":[], "if":"true":[], "(with* ([x (+ 1 2)] [y (* x x)] [x (+ y 3)]) (+ x y))", "pass"), 
  ("multiple bindings", "+":"*":[], "if":"true":[], "(with* ([x (+ 1 2)] [y (* x x)] [x (+ y 3)]) (+ x y))", "pass"),
  ("unbound z", "+":"*":[], "if":"true":[], "(with* ([x (+ 1 2)] [y (* x x)] [x (+ y 3)]) (+ z y))", "unbound"),
  ("bound reserved word if", "+":"*":[], "if":"true":[], "(with* ([if (+ 1 2)] [y (* 2 3)] [x (+ y 3)]) (+ x y))", "reserved"),
  ("bound reserved word in if ", "+":"*":[], "if":"true":[], "(if (< (* 2 3) (+ (+ 5 1) (+ 5 1) )) 7 10)", "unbound"),
  ("unbound in body",  "+":"*":[], "if":"true":[], "(fun (x) (+ 2 y))", "unbound")
  ]  		

testCheckId (description, bound, reserved, str, expected) =
  (description,
   case parseSExp (tokenize str) of
    Ok (sexp, []) -> case parseExpr sexp of
      Ok(expr) -> case desugar expr of 
        Ok(dexpr) -> case checkIds bound reserved dexpr of
        	Ok(x) -> expected == "pass"
        	Err(x) -> isInfixOf expected x == True

  )			     							  