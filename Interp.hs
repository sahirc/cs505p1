module InterpFun where

import Expr
import Result
import SExp

-- Values resulting from interpreting an expression.
data Val = NumV Integer
         | BoolV Bool
         | FunV Var CExpr Env
         | PrimV String (Val -> Result Val)  -- name and implementation

type Env = [(Var, Val)]

instance Show Val where
  show (NumV n) = show n
  show (BoolV b) = show b
  show (FunV var body env) = "(fun (" ++ var ++ ") " ++ (show body) ++ " | " ++
                             (show env) ++ ")"
  show (PrimV name impl) = "<primitive: " ++ name ++ ">"

wrapBinaryArithOp :: String -> (Integer -> Integer -> Val) -> Val
wrapBinaryArithOp name op =
  PrimV name (
    \arg1 -> return (PrimV ("partial:" ++ name)
                     (\arg2 ->
                       case (arg1, arg2) of
                        (NumV lv, NumV rv) -> return (op lv rv)
                        nonNum -> fail ("numeric op applied to: " ++
                                        (show nonNum)))))

-- Populate initialEnv ...
initialEnv :: Env
initialEnv = [ ("+", wrapBinaryArithOp "add" add),
               ("*", wrapBinaryArithOp "mult" mult),
               ("=", wrapBinaryArithOp "equals" equals),
               ("<", wrapBinaryArithOp "lessThan" lessThan),
               ("true", BoolV True),
               ("false", BoolV False)
             ]

add :: Integer -> Integer -> Val
add l r = NumV (l + r)

mult :: Integer -> Integer -> Val
mult l r = NumV (l * r)

equals :: Integer -> Integer -> Val
equals l r = BoolV (l == r)

lessThan :: Integer -> Integer -> Val
lessThan l r = BoolV (l < r)

interp :: CExpr -> Env -> Result Val
interp expr env = case expr of 
                    NumC i -> Ok(NumV i)
                    VarC v -> case lookup v env of  
                                Nothing -> Err "unbound var"
                                Just(v) -> Ok(v) 
                    IfC cond cons alt -> case (interp cond env) of 
                                            Ok(BoolV True) -> (interp cons env)
                                            Ok(BoolV False) -> (interp alt env)
                    FunC var cexpr -> Ok(FunV var cexpr env)
                    AppC c1 c2 -> case (interp c1 env) of
                                    Ok(FunV v c3 env') -> interp c3 {-[v,c2i] ++ -}env'
                                    Ok(PrimV name f) -> f (c2i)
                                    where c2i = case interp c2 env of 
                                                  Ok(x) -> x