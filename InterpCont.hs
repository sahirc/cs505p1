module InterpCont where

import Expr
import Result
import SExp
import Token

-- Values resulting from parseCheckAndInterpStreting an expression.
data Val = NumV Integer -- a numeric constant
         | BoolV Bool -- a boolean constant
         | StringV String -- a string constant
         | EmptyV -- an empty list
         | ConsV Val Val -- a non-empty list
         | PairV Val Val -- a pair
         | FunV Var CExpr Env -- a function closure
         | PrimV String (Val -> Cont -> Result Val)  -- primitive: name and implementation

type Env = [(Var, Val)]

instance Show Val where
  show (NumV n) = show n
  show (BoolV b) = show b
  show (StringV s) = show s
  show EmptyV = "empty"
  show (ConsV h t) = "(cons " ++ (show h) ++ " " ++ (show t) ++ ")"
  show (PairV f s) = "(" ++ (show f) ++ ", " ++ (show s) ++ ")"
  show (FunV var body _) = "(fun (" ++ var ++ ") " ++ (show body) ++ ")"
  show (PrimV name impl) = "<primitive: " ++ name ++ ">"

data Cont = DoneK
          | IfK CExpr CExpr Env Cont
          | AppFunK CExpr Env Cont
          | AppArgK Val Cont
          | CustomK Val Cont
          | ContextK Val Cont
          deriving Show

handleError :: Cont -> Val -> Result Val
handleError k val = case k of 
                      AppArgK _ b  -> case b of
                                  AppArgK arg cont -> apply arg val k
                                  CustomK handler cont -> apply handler val cont

wrapBinaryArithOp :: String -> (Integer -> Integer -> Val) -> Val
wrapBinaryArithOp name op =
  PrimV name (
    \arg1 k -> callK k (PrimV ("partial:" ++ name)
                        (\arg2 k ->
                          case (arg1, arg2) of
                           (NumV lv, NumV rv) -> callK k (op lv rv)
                           nonNum -> handleError k (StringV (name ++ " applied to: " ++
                                                             (show nonNum))))))

add = wrapBinaryArithOp "+" (\x y -> (NumV (x+y)))
mult = wrapBinaryArithOp "*" (\x y -> (NumV (x*y)))
equal = wrapBinaryArithOp "=" (\x y -> (BoolV (x == y)))
less = wrapBinaryArithOp "<" (\x y -> (BoolV (x < y)))

unimplemented name = PrimV name (\v k -> Err (name ++ ": unimplemented"))

emptyP = PrimV "empty?" (\list k -> case list of 
                            EmptyV -> callK k (BoolV True)
                            _      -> callK k (BoolV False)
                        ) 

consP = PrimV "cons?" (\list k -> case list of 
                            ConsV a b  -> callK k (BoolV True)
                            _          -> callK k (BoolV False)
                      ) 

cons = PrimV "cons" (\l k -> callK k (PrimV ("partial:" ++ "cons") 
                                        (\r k -> case r of
                                                   ConsV _ _ -> callK k (ConsV l r)
                                                   EmptyV -> callK k (ConsV l r)
                                                   _ -> handleError k (StringV "Can't cons onto a nonList")
                                        )
                                      )
                    )

first = PrimV "first" (\list k -> case list of 
                                    ConsV l r -> callK k l
                                    _ -> handleError k (StringV "Can't first a nonList"))

rest = PrimV "rest" (\list k -> case list of
                                    ConsV l r -> callK k r
                                    _ -> handleError k (StringV "Can't rest a nonList"))

pair = PrimV "pair" (\l k -> callK k (PrimV ("partial:" ++ "cons") 
                                        (\r k -> callK k (PairV l r)
                                        )))
pairFst = PrimV "fst" (\p k -> case p of 
                                  PairV l _ -> callK k l
                                  _ -> handleError k (StringV "Can't get fst of a nonPair"))
                                        
pairSnd = PrimV "snd" (\p k -> case p of 
                                  PairV _ r -> callK k r
                                  _ -> handleError k (StringV "Can't get snd of a nonPair"))

raise = PrimV "raise" (\arg k -> handleError k arg)

callWithHandler = PrimV "call-with-handler" (\thunk k -> callK k (PrimV "handler" (
                                              \handler k -> apply thunk EmptyV (
                                                (CustomK handler k)))))
                                                            
callWithContext = PrimV "call-with-context" (\ctx k -> 
                                              callK k (
                                                PrimV "thunk" (\thunk k -> apply thunk EmptyV (ContextK ctx k))))

getContext = PrimV "get-context" (\key k -> callK k (getContextHelper k))
                             
getContextHelper :: Cont -> Val
getContextHelper cont = case cont of
                          DoneK -> EmptyV
                          ContextK val k' -> ConsV val (getContextHelper k')
                          AppArgK v c -> getContextHelper c
                          AppFunK _ _ k -> getContextHelper k


callCc = unimplemented "call/cc"

bind prim@(PrimV name fn) = (name, prim)
bind nonPrim = error ("cannot bind " ++ (show nonPrim))

-- Populate initialEnv ...
initialEnv :: Env
initialEnv = [
  ("true", BoolV True),
  ("false", BoolV False),
  ("empty", EmptyV)] ++
  (map bind [add, mult, equal, emptyP, first, rest, cons,
             consP, pair, pairFst, pairSnd, callCc, callWithContext,
             getContext, callWithHandler, raise])

interp :: CExpr -> Env -> Cont -> Result Val
interp expr env k =
  case expr of
   NumC n -> callK k (NumV n)
   StringC s -> callK k (StringV s)
   FunC var body -> callK k (FunV var body env)
   VarC v ->
     case lookup v env of
      Nothing -> Err ("unbound id: " ++ v)
      Just val -> callK k val
   IfC cond cons alt -> interp cond env (IfK cons alt env k)
   AppC fun arg -> interp fun env (AppFunK arg env k)

callK :: Cont -> Val -> Result Val
callK k val =
  case k of
   DoneK -> Ok val
   IfK cons alt env k ->
     case val of
      BoolV True -> interp cons env k
      BoolV False -> interp alt env k
      nonBool -> Err ("`if` expected bool, got: " ++ (show nonBool))
   AppFunK arg env k -> interp arg env (AppArgK val k)
   AppArgK arg k ->  apply arg val k
   CustomK _ cont -> callK cont val
   ContextK _ cont -> callK cont val

parseCheckAndInterpStr :: String -> Result Val
parseCheckAndInterpStr str =
  let toks = tokenize str
      initialIds = map fst initialEnv in
  do (sexp, _) <- parseSExp toks
     expr <- parseExpr sexp
     cexp <- desugar expr
     checkIds initialIds (["fun", "if", "with*"] ++ initialIds) cexp
     interp cexp initialEnv DoneK

apply :: Val -> Val -> Cont -> Result Val
apply (FunV var cexpr env) val k = interp cexpr ((var, val):env) k
apply (PrimV name f) arg k = f arg k

apply _ _ _ = fail "Not a FunV or PrimV"