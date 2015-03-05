module TypeCheck (checkType, parseAndCheckStr) where

import Data.Char
import Data.List
import Expr
import Result
import SExp -- used only by parseAndCheckStr
import Token -- used only by parseAndCheckStr

-- A typing context includes a set of polymorphic type variables that
-- are in scope, along with bindings of program variables to their
-- declared types.
type TyContext = ([TVar], [(TVar, Type)])

merge [] ys = ys
merge (x:xs) ys = x:merge ys xs

-- Problem 2.
freeTypeVars :: Type -> [TVar] -> [TVar]
freeTypeVars ty bound = case ty of 
                          ForAllT tvar ty' -> freeTypeVars ty' (tvar:bound)
                          ListT t1 -> freeTypeVars t1 bound
                          ArrowT t1 t2 -> merge (freeTypeVars t1 bound)  (freeTypeVars t2 bound)
                          PairT t1 t2 -> merge (freeTypeVars t1 bound)  (freeTypeVars t2 bound)
                          VarT tvar -> if elem tvar bound
                                         then
                                           []   
                                         else
                                           [tvar]
                          _ -> []

-- Problem 3.
alphaRename :: TVar -> TVar -> Type -> Type
alphaRename vIn vOut (VarT t) | t == vIn = VarT vOut
                              | otherwise = VarT vIn
alphaRename vIn vOut (ForAllT t ty) | t == vIn = ForAllT vOut (alphaRename vIn vOut ty)
                                    | otherwise = ForAllT vIn (alphaRename vIn vOut ty)
alphaRename vIn vOut (PairT l r) = PairT (alphaRename vIn vOut l) (alphaRename vIn vOut r)
alphaRename vIn vOut (ListT t) = ListT (alphaRename vIn vOut t)                                            
alphaRename vIn vOut (ArrowT l r) = ArrowT (alphaRename vIn vOut l) (alphaRename vIn vOut r)
alphaRename vIn vOut ty = ty

-- Implementation complete: nothing to do here. Use this helper in checkType.
checkClosed :: Type -> [TVar] -> Result ()
checkClosed ty bound =
  case freeTypeVars ty bound of
   [] -> Ok ()
   nonEmpty -> Err ("unbound type var(s) in " ++ (show ty) ++ ": " ++ (show nonEmpty))

-- Problem 4.
subst :: TVar -> Type -> Type -> Type
subst var forType inType = inType -- implement me!

-- Problem 5.
checkType :: DExpr -> TyContext -> Result Type
checkType expr gamma = Err "implement me!"

-- Implementation complete.
-- Generates a variable name that's distinct from every name in the argument list.
-- Use this helper for alpha-renaming in subst.
genFreshVar :: [String] -> String
genFreshVar [] = "a"
genFreshVar tabu = reverse (inc (reverse (maximum tabu)))
  where inc ('z':cs) = 'a':'z':cs
        inc (c:cs) = (succ c):cs

-- Implementation complete: nothing to do here. Use this helper for alpha-renaming in subst.
allTypeVars :: Type -> [TVar]
allTypeVars (ArrowT t1 t2) = (allTypeVars t1) ++ (allTypeVars t2)
allTypeVars (ListT t1) = allTypeVars t1
allTypeVars (PairT t1 t2) = (allTypeVars t1) ++ (allTypeVars t2)
allTypeVars (VarT v) = [v]
allTypeVars (ForAllT v t) = [v] ++ (allTypeVars t)
allTypeVars NumT = []
allTypeVars BoolT = []
allTypeVars StringT = []

initialTypeEnv :: [(Var, Type)]
initialTypeEnv = [
  ("true", BoolT),
  ("false", BoolT),
  ("+", ArrowT NumT (ArrowT NumT NumT)),
  ("*", ArrowT NumT (ArrowT NumT NumT)),
  ("=", ArrowT NumT (ArrowT NumT BoolT)),
  ("<", ArrowT NumT (ArrowT NumT BoolT)),
  ("cons", ForAllT "a" (ArrowT (VarT "a")
                        (ArrowT (ListT (VarT "a")) (ListT (VarT "a"))))),
  ("empty", ForAllT "a" (ListT (VarT "a"))),
  ("first", ForAllT "a" (ArrowT (ListT (VarT "a")) (VarT "a"))),
  ("rest", ForAllT "a" (ArrowT (ListT (VarT "a")) (ListT (VarT "a")))),
  ("empty?", ForAllT "a" (ArrowT (ListT (VarT "a")) BoolT)),
  ("cons?", ForAllT "a" (ArrowT (ListT (VarT "a")) BoolT)),
  ("pair", ForAllT "a" (ForAllT "b" (ArrowT (VarT "a") (ArrowT (VarT "b")
                                                        (PairT (VarT "a") (VarT "b")))))),
  ("fst", ForAllT "a" (ForAllT "b" (ArrowT (PairT (VarT "a") (VarT "b"))
                                    (VarT "a")))),
  ("snd", ForAllT "a" (ForAllT "b" (ArrowT (PairT (VarT "a") (VarT "b"))
                                    (VarT "b")))),
  ("fix", ForAllT "a" (ArrowT (ArrowT (VarT "a") (VarT "a")) (VarT "a"))),
  ("call/cc", ForAllT "a" (ArrowT (ArrowT (ArrowT (VarT "a") (ForAllT "b" (VarT "b")))
                                   (VarT "a"))
                           (VarT "a")))
  ]

parseAndCheckStr :: String -> Result Type
parseAndCheckStr str =
  let toks = tokenize str in
  do (sexp, _) <- parseSExp toks
     expr <- parseExpr sexp
     cexp <- desugar expr
     checkType cexp ([], initialTypeEnv)