module SExp (SExp(NumS, IdS, ListS), parseSExp, tokenize, Result(Ok, Err)) where

import Token
import Data.Maybe

-- Converts a string into a list of tokens.
tokenize :: String -> [Token]
tokenize str
    | isNothing(token) = []
    | otherwise = [fst(fromJust(token))] ++ tokenize(snd(fromJust(token)))
    where token = parseToken str

-- S-expression data definition.
data SExp = NumS Integer -- numeric expression
          | IdS String -- identifier
          | ListS [SExp] -- list of S-expressions
          deriving Eq

-- Show SExps in surface syntax.
instance Show SExp where
  show (NumS n) = show n
  show (IdS name) = name
  show (ListS sexps) = "(" ++ (unwords (map show sexps)) ++ ")"


-- Type for results of functions that can fail, such as parsing.
data Result a = Ok a -- Success
              | Err String -- Error with description
              deriving (Eq, Show)

-- Attempts to parse an S-expression from a list of tokens.
-- If successful, returns:
--    Ok (<parsed S-expression>, <remaining tokens>)
-- If not, returns:
--    Err <string describing problem encountered>
parseSExp :: [Token] -> Result (SExp, [Token])
parseSExp [] = Err "Can't parse an empty string"
parseSExp all@(token:tokens) = 
    case token of
      NumTok t -> Ok(NumS(t), tokens)
      IdTok t -> Ok(IdS(t), tokens)
      Open t -> parseHelper t [] tokens
      Close t -> Err "Close brackets should be handle as tokens, they should always be skipped"

parseHelper :: Brace -> [SExp] -> [Token] -> Result(SExp, [Token])
parseHelper obrace accum tokens =
  case tokens of 
    [] -> Err "Shouldn't have an empty list"
    Close cbrace:ts' | obrace == cbrace  -> Ok(ListS (reverse accum), ts')
                     | otherwise -> Err "not a matching bracket"
    _ -> case parseSExp(tokens) of
          Ok(sexp, t') -> parseHelper obrace (sexp:accum) t'
          Err _ -> Err "something went wrong"
      
-- Examples that should parse.
validExamples = [
  ("empty list", "()", ListS []),
  ("single id", "true", IdS "true"),
  ("positive num", "1234", NumS 1234),
  ("negative num", "-1234", NumS (-1234)),
  ("mixed list", "(foo () 4 (7 false))", ListS [IdS "foo", ListS [], NumS 4, ListS [NumS 7, IdS "false"]])
  ]

-- Examples that should not parse.
invalidExamples = [
  ("empty", ""),
  ("close without open", ")"),
  ("no close", "(3 4"),
  ("mismatched brace types", "(foo bar]")
  ]

-- Check a single valid example.
checkValid (description, input, expected) =
  (description, (parseSExp (tokenize input)) == Ok (expected, []))

-- Check a single invalid example.
checkInvalid (description, input) =
  (description, case parseSExp (tokenize input) of
                 -- Just check that it failed; don't try to match the
                 -- specific error message.
                 Err _ -> True
                 _ -> False)
