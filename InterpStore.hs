module InterpStore where

import Control.Applicative
import Control.Monad
import Expr
import Result
import SExp

type Loc = Int

type Store = (Loc, [(Loc, Val)])

-- Store transformer that carries a Result of type a. If we get an
-- error, both the error and the store (at the point where the error
-- occurred) are propagated; the contents of the store may be useful
-- for debugging.
newtype STR a = STR (Store -> (Result a, Store))


wrapBinaryArithOp :: String -> (Integer -> Integer -> Val) -> Val
wrapBinaryArithOp name op =
  PrimV name (
    \arg1 -> return (PrimV ("partial:" ++ name)
                     (\arg2 ->
                       case (arg1, arg2) of
                        (NumV lv, NumV rv) -> return (op lv rv)
                        nonNum -> fail ("numeric op applied to: " ++
                                        (show nonNum)))))
-- Populate initialEnv...
initialEnv :: Env
initialEnv = [ ("+", wrapBinaryArithOp "add" add),
               ("*", wrapBinaryArithOp "mult" mult),
               ("=", wrapBinaryArithOp "equals" equals),
               ("<", wrapBinaryArithOp "lessThan" lessThan),
               ("true", BoolV True),
               ("false", BoolV False),
               ("box", box),
               ("set-box!", setbox),
               ("unbox", unbox)
             ]

-- Values resulting from interpreting an expression.
data Val = NumV Integer
         | BoolV Bool
         | FunV Var CExpr Env
         | PrimV String (Val -> STR Val)
         | BoxV Loc

box :: Val
box = PrimV "box'" (\val -> alloc val >>= (\loc -> return(BoxV(loc))))

unbox :: Val
unbox = PrimV "unbox" (\val -> case val of 
                                    BoxV loc -> fetch loc
                                    nonBox -> fail("non box" ++ show(nonBox)))

setbox :: Val
setbox = PrimV "setbox" (\arg1 -> return (PrimV "something"
                            (\arg2 -> case arg1 of 
                                BoxV loc -> update loc arg2 >>= (\_ -> return (arg2)))
                            )
                        )

alloc :: Val -> STR Loc
alloc val = STR (\store -> ( Ok (fst (store)), 
                               (fst (store) + 1, (fst store, val) : (snd (store)) 
                               ) 
                          )
                )

fetch :: Loc -> STR Val
fetch loc = STR (\arg1 -> case lookup loc (snd arg1) of
                            Nothing -> (Err("Nothing found for " ++ show(loc)), arg1)
                            Just val -> (Ok (val), arg1)
                )

update :: Loc -> Val -> STR ()
update loc val = STR (\arg1 -> case lookup loc (snd arg1) of
                            Nothing -> (Err("Cannot update that which does not exist " ++ show(loc)), arg1)
                            Just val' -> (Ok (), (fst(arg1), (loc, val):(snd arg1) )))

add :: Integer -> Integer -> Val
add l r = NumV (l + r)

mult :: Integer -> Integer -> Val
mult l r = NumV (l * r)

equals :: Integer -> Integer -> Val
equals l r = BoolV (l == r)

lessThan :: Integer -> Integer -> Val
lessThan l r = BoolV (l < r)

instance Functor STR where
  fmap f st = st >>= return . f

instance Monad STR where
  -- (>>=) :: STR a -> (a -> STR b) -> STR b
  (STR st) >>= f = STR (\store -> case st store of 
                                    (Ok result, store') -> case f (result) of 
                                      STR b -> b store' 
                                    (Err msg, store) -> (Err(msg), store)
                       )

  -- return :: a -> STR a
  return v = STR (\s -> (Ok v, s))
  fail msg = STR (\s -> (Err msg, s))

instance Applicative STR where
  pure = return
  (<*>) = ap

type Env = [(Var, Val)]

instance Show Val where
  show (NumV n) = show n
  show (BoolV b) = show b
  show (FunV var body env) = "(fun (" ++ var ++ ") " ++ (show body) ++ " | " ++
                             (show (map fst env)) ++ ")"
  
  show (PrimV name impl) = "<primitive: " ++ name ++ ">"
  show (BoxV loc) = "<box@" ++ (show loc) ++ ">"

runSTR :: Store -> STR a -> (Result a, Store)
runSTR s (STR st) = st s

interp :: CExpr -> Env -> STR Val
interp expr env = case expr of 
                    NumC i -> return (NumV i)
                    VarC v -> case lookup v env of  
                                Nothing -> fail (show(v) ++ " is an unbound var")
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
                                  