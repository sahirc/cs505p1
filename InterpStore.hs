module InterpStore where

import Control.Applicative
import Control.Monad
import Expr
import Result
import SExp
import Data.List

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

alloc :: Val -> STR Loc
alloc val = STR (\arg1 -> ( Ok (fst (arg1) + 1), 
                               (fst (arg1) + 1, (fst arg1, val) : (snd (arg1)) 
                               ) 
                          )
                )  
  
fetch :: Loc -> STR Val
fetch loc = STR (\arg1 -> case lookup loc (snd arg1) of
                            Nothing -> (Err("Nothing found for " ++ show(loc)), arg1)
                            Just val -> (Ok (val), arg1)
                )

copy :: (Loc, Val) -> [(Loc, Val)] -> [(Loc, Val)]
copy new [] = []
copy new (old:olds) = if(fst(old) == fst(new)) then
                          (fst(new), snd(new)) : copy new olds
                        else
                          (fst(old), snd(old)) : copy new olds

update :: Loc -> Val -> STR ()
update loc val = STR (\arg1 -> case lookup loc (snd arg1) of
                            Nothing -> (Err("Cannot update that which does not exist " ++ show(loc)), arg1)
                            Just val -> (Ok (), (fst(arg1), copy (loc, val) (snd arg1) )))

add :: Integer -> Integer -> Val
add l r = NumV (l + r)

mult :: Integer -> Integer -> Val
mult l r = NumV (l * r)

equals :: Integer -> Integer -> Val
equals l r = BoolV (l == r)

lessThan :: Integer -> Integer -> Val
lessThan l r = BoolV (l < r)

box :: Val -> Val
box val = case alloc val of 
            STR x -> error ("is this right")

instance Functor STR where
  fmap f st = st >>= return . f

instance Monad STR where
  -- (>>=) :: STR a -> (a -> STR b) -> STR b
  (STR st) >>= f = fail "(>>=) not yet implemented for STR"

  -- return :: a -> STR a
  return v = fail "'return' not yet implemented for STR"

  fail msg = STR (\s -> (Err msg, s))

instance Applicative STR where
  pure = return
  (<*>) = ap

-- Values resulting from interpreting an expression.
data Val = NumV Integer
         | BoolV Bool
         | FunV Var CExpr Env
         | PrimV String (Val -> STR Val)
         | BoxV Loc

type Env = [(Var, Val)]

instance Show Val where
  show (NumV n) = show n
  show (BoolV b) = show b
  show (FunV var body env) = "(fun (" ++ var ++ ") " ++ (show body) ++ " | " ++
                             (show (map fst env)) ++ ")"
  
  show (PrimV name impl) = "<primitive: " ++ name ++ ">"
  show (BoxV loc) = "<box@" ++ (show loc) ++ ">"

-- Populate initialEnv...
initialEnv :: Env
initialEnv = [ ("+", wrapBinaryArithOp "add" add),
               ("*", wrapBinaryArithOp "mult" mult),
               ("=", wrapBinaryArithOp "equals" equals),
               ("<", wrapBinaryArithOp "lessThan" lessThan),
               ("true", BoolV True),
               ("false", BoolV False)
               --("box", box)
             ]

interp :: CExpr -> Env -> STR Val
interp expr env = fail "'interp' not yet implemented"