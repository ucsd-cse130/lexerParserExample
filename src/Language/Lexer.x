{
{-# LANGUAGE FlexibleContexts #-}

module Language.Lexer where

import Control.Monad.Except
}

%wrapper "posn"



tokens :-
  -- Whitespace insensitive
  \n                            ;
  $white+                       ;

-- REGULAR_EXPRESSION { \ALEX_POSITION MATCHED_STRING -> LEXER_TOKEN }
  \+      { \p _ -> LPLUS p }
  \*      { \p _ -> LMUL  p }
  [0-9]+  { \p s -> LINT  p (read s) }
{
data LToken
  = LINT    AlexPosn Int
  | LPLUS   AlexPosn
  | LMUL   AlexPosn
  deriving (Eq, Show)



scanTokens :: String -> Except String [LToken]
scanTokens str = go (alexStartPos,'\n',[],str)
  where
    go inp@(pos,_,_,str) =
      case alexScan inp 0 of
        AlexEOF -> return []
        AlexError ((AlexPn _ line column),_,_,_) -> throwError $ "lexical error at " ++ (show line) ++ " line, " ++ (show column) ++ " column"
        AlexSkip  inp' _       -> go inp'
        AlexToken inp' len act -> do
          res <- go inp'
          let rest = act pos (take len str)
          return (rest : res)
}
