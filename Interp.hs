module InterpFun where

import Expr
import Result
import SExp

-- Values resulting from interpreting an expression.
data Val = NumV Integer
         | BoolV Bool
         | FunV Var CExpr Env
         | PrimV String (Val -> Result Val) -- name and implementation

type Env = [(Var, Val)]

instance Eq Val where
  NumV a == NumV b = a == b

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
                    NumC i -> return (NumV i)
                    VarC v -> case lookup v env of  
                                Nothing -> fail (show(v) ++ " is an unbound var in " ++ show(env))
                                Just(v) -> return v 
                    IfC cond cons alt ->  interp cond env >>= (\condVal ->
                                            case condVal of 
                                              BoolV True -> interp cons env
                                              BoolV False -> interp alt env
                                              nonBool -> fail ("Need a BoolV, received a " ++ show(nonBool))
                                          )
                    FunC var cexpr -> return (FunV var cexpr env)
                    AppC c1 c2 -> interp c1 env >>= (\f1 ->
                                    case f1 of
                                      (FunV v c3 env') -> interp c2 env >>= (\f2 -> interp c3 ((v,f2) : env'))  
                                      (PrimV name f) -> interp c2 env >>= f
                                      nonFun -> fail ("Need a PrimV for a FunV, received a " ++ show(nonFun))
                                      )
interpTestCases = [
  ("with from hw", initialEnv, "(with* ([x (+ 1 2)] [y (* x x)] [x (+ y 3)]) (+ x y))", Ok(NumV 21)),
  ("if", initialEnv, "(if (< (* 5 3) (+ (+ 5 1) (+ 5 1) )) 7 10)", Ok(NumV 10)),
  ("unbound var", initialEnv, "(with* ([x (+ 1 2)] [y (* x x)] [x (+ y 3)]) (+ z y))", Err "some error")
  ]     

testInterp (description, env, str, expected) =
  (description,
   case parseSExp (tokenize str) of
    Ok (sexp, []) -> case parseExpr sexp of
      Ok(expr) -> case desugar expr of 
        Ok(dexpr) -> case interp dexpr env of
          Ok(val) -> Ok(val) == expected
          Err(_) -> Err("some error") == expected
        Err(_) -> Err("some error") == expected

  )