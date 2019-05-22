{
module Language.Parser where

import Language.Lexer
import Language.Types
import Control.Monad.Except
import Control.Exception
import Data.Typeable
}

%name top
%tokentype { LToken }
%monad { Except String } { (>>=) } { return }
%error { parseError }



%token
    PINT  { LINT _ $$ }
    PPLUS { LPLUS _ }
    PMUL { LMUL _ }
%left PPLUS
%left PMUL
%%

PExpr : PINT               { EInt $1 }
      | PExpr PMUL PExpr   { EMul $1 $3 }
      | PExpr PPLUS PExpr  { EPlus $1 $3 }



{
data Error = Error {errMsg :: String}
             deriving (Show, Typeable)
instance Exception Error

parseError :: [LToken] -> Except String a
parseError (l:ls) = throwError (show l)
parseError []     = throwError "Unexpected end of Input"

parseExpr :: String -> Expr
parseExpr s = case parseExpr' s of
                Left msg -> throw (Error ("parse error:" ++ msg))
                Right e  -> e

parseExpr' input = runExcept $ do
   tokenStream <- scanTokens input
   top tokenStream

parseTokens :: String -> Either String [LToken]
parseTokens = runExcept . scanTokens
}
