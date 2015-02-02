module Result (Result(..)) where

import Control.Applicative
import Control.Monad

-- Type for results of functions that can fail, such as parsing.
data Result a = Ok a -- Success
              | Err String -- Error with description
              deriving (Eq, Show)

instance Functor Result where
  fmap f v = v >>= (return . f)

instance Monad Result where
  (Ok v) >>= f = f v
  (Err msg) >>= _  = Err msg
  return = Ok
  fail = Err

instance Applicative Result where
  pure = return
  (<*>) = ap