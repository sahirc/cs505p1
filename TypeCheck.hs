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
freeTypeVars (VarT v) bound | v `elem` bound = []
                            | otherwise = [v]
freeTypeVars (ForAllT v ty)     bound = freeTypeVars ty (v:bound)
freeTypeVars (ArrowT t1 t2)     bound = merge (freeTypeVars t1 bound)  (freeTypeVars t2 bound)
freeTypeVars (PairT t1 t2)      bound = merge (freeTypeVars t1 bound)  (freeTypeVars t2 bound)
freeTypeVars (ListT t)          bound = freeTypeVars t bound
freeTypeVars t bound = []

-- Problem 3.
alphaRename :: TVar -> TVar -> Type -> Type
alphaRename vIn vOut (VarT t) | t == vIn = VarT vOut
                              | otherwise = VarT t
alphaRename vIn vOut (ForAllT t ty) | t == vIn = ForAllT vOut (alphaRename vIn vOut ty)
                                    | otherwise = ForAllT vIn (alphaRename vIn vOut ty)
alphaRename vIn vOut (PairT l r)  = PairT (alphaRename vIn vOut l) (alphaRename vIn vOut r)
alphaRename vIn vOut (ListT t)    = ListT (alphaRename vIn vOut t)                                            
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
subst var forType (VarT var') | var' == var = forType
subst var forType (ArrowT l r) = ArrowT (subst var forType l) (subst var forType r)
subst var forType (PairT l r) = PairT (subst var forType l) (subst var forType r)
subst var forType (ListT t) = ListT (subst var forType t)
subst var forType (ForAllT t ty) | t `elem` (allTypeVars forType) = ForAllT newVar (subst var forType (alphaRename t newVar ty))
                                 | t == var = ForAllT t ty
                                 | otherwise = ForAllT t (subst var forType ty)
                                   where newVar = genFreshVar (allTypeVars forType) -- TODO: are we passing in the right argument list
-- NumT, StringT, BoolT
subst var forType inType = inType

-- Problem 5.
checkType :: DExpr -> TyContext -> Result Type
checkType (NumD _) _ = return NumT
checkType (StringD _) _ = return StringT
checkType (IfD cond cons alt) gamma = checkType cond gamma >>= (\tyCond ->
                                        case tyCond of 
                                          BoolT -> checkType cons gamma >>= (\tyCons ->
                                            checkType alt gamma >>= (\tyAlt -> 
                                              if doTypesMatch tyAlt tyCons
                                                then return tyAlt -- they match, so you can return either
                                                else fail "cons and alt don't return same type"))
                                          _ -> fail "IfD cons did not type to bool"
                                          )
-- Return the type of the variable
-- Only if it's closed in the bound types
checkType (VarD v) (bt, pt) = case lookup v pt of
                                Just t -> case checkClosed t bt of 
                                            Ok(_) -> return t
                                            _ -> fail (show t ++ " is not closed in " ++ show bt)
                                _ -> fail ("Don't know what " ++ show v ++ " is")
-- Return an ArrowT from t to the type of dexpr
checkType (FunD v t dexpr) (bt, pt) = checkType dexpr (v:bt, (v, t):pt) >>= (\dexpr' -> 
                                        return (ArrowT t dexpr')
                                      )
-- Assert that the input of d1 matches d2
checkType (AppD d1 d2) gamma = checkType d1 gamma >>= (\tyd1 -> 
                                 case tyd1 of
                                   ArrowT t1 t2 -> 
                                    checkType d2 gamma >>= (\tyd2 -> 
                                      if doTypesMatch t1 tyd2
                                        then return t2
                                        else fail "types' dont match" 
                                    )
                                   ForAllT tvar ty -> 
                                     checkType d2 gamma >>= (\tyd2 -> 
                                       if doTypesMatch ty tyd2
                                         then return ty
                                         else fail ("types don't match" ++ show tyd2)
                                      )
                                   _ -> fail ("An AppD must have an ArrowT as it's initial dexpr" ++ show tyd1)
                                )
checkType (ForAllD v d) (bt,pt) =  checkType d (v:bt, pt) >>= (\ty -> return (ForAllT v ty))
checkType (WithD v d1 d2) gamma@(bt, pt) =  checkType d1 gamma >>= (\tyd1 -> checkType d2 (v:bt, (v, tyd1):pt) )
checkType (SpecD d t) gamma = checkType d gamma >>= (\tyd -> 
                                case tyd of  
                                  ForAllT v ty -> return (subst v t ty)
                                  _ -> fail "Spec can only be applied to a ForAll"
                              )
doTypesMatch :: Type -> Type -> Bool
doTypesMatch  t1 t2 | t1 == t2 = True
                    | otherwise = False
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