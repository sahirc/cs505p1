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
               ("box", box'),
               ("set-box!", setbox'),
               ("unbox", unbox fetch)
             ]

-- Values resulting from interpreting an expression.
data Val = NumV Integer
         | BoolV Bool
         | FunV Var CExpr Env
         | PrimV String (Val -> STR Val)
         | BoxV Loc

box' :: Val
box' = PrimV "box'" (\val -> alloc val >>= (\loc -> return(BoxV(loc))))

unbox :: (Loc -> STR Val) -> Val
unbox fetch = PrimV "unbox" (\val -> case val of BoxV loc -> fetch loc) -- handle !box

alloc :: Val -> STR Loc
alloc val = STR (\store -> ( Ok (fst (store)), 
                               (fst (store) + 1, (fst store, val) : (snd (store)) 
                               ) 
                          )
                )

setbox' :: Val
setbox' = PrimV "setbox" (\arg1 -> return (PrimV "something"
                            (\arg2 -> case arg1 of BoxV loc -> update loc arg2 >>= (\_ -> return (arg2)))))

fetch :: Loc -> STR Val
fetch loc = STR (\arg1 -> case lookup loc (snd arg1) of
                            Nothing -> (Err("Nothing found for " ++ show(loc)), arg1)
                            Just val -> (Ok (val), arg1)
                )

update :: Loc -> Val -> STR ()
update loc val = STR (\arg1 -> case lookup loc (snd arg1) of
                            Nothing -> (Err("Cannot update that which does not exist " ++ show(loc)), arg1)
                            Just val' -> (Ok (), (fst(arg1), (loc, val):(snd arg1) )))

copy :: (Loc, Val) -> [(Loc, Val)] -> [(Loc, Val)]
copy new [] = []
copy new (old:olds) = if(fst(old) == fst(new)) then
                          (fst(new), snd(new)) : copy new olds
                        else
                          (fst(old), snd(old)) : copy new olds

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
                                      (result, store) -> case result of 
                                        Err(msg) -> (Err(msg), store)
                                        Ok(a) -> case f (a) of
                                          STR b -> b store
                                      )

  -- return :: a -> STR a
  return v = STR (\store -> (Ok(v), store))

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

interp :: CExpr -> Env -> STR Val
interp expr env = case expr of 
                    NumC i -> return (NumV i)
                    VarC v -> case lookup v env of  
                                Nothing -> fail (show(v) ++ " is an unbound var")
                                Just(v) -> return v 
                    IfC cond cons alt ->  interp cond env >>= (\cond ->
                                            case cond of 
                                              BoolV True -> interp cons env
                                              BoolV False -> interp alt env
                                              x -> return x
                                          )
                    FunC var cexpr -> return (FunV var cexpr env)
                    AppC c1 c2 -> interp c1 env >>= (\f1 ->
                                    case f1 of
                                      (FunV v c3 env') -> interp c2 env >>= (\f2 -> interp c3 ((v,f2) : env'))  
                                      (PrimV name f) -> interp c2 env >>= f
                                      )
                                  