-- Advanced Programming, Final Project
-- by Jason Mow (jmow), Kyle Hardgrave (kyleh)

{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults  #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE TypeSynonymInstances, FlexibleContexts, NoMonomorphismRestriction, FlexibleInstances #-}

module Parser (GenParser, Parser, 
                   getC,
                   choose,
                   (<->),
                   satisfy,
                   doParse,  
                   ) where


import Control.Monad.List()
import Control.Monad.State

-- StateT (String -> [(a,String)])
type GenParser e a = StateT [e] [] a

instance (Show e, Show a) => Show (GenParser e a) where
  show m = show $ runStateT m []

type Parser a = GenParser Char a

doParse :: GenParser e a  -> [e] -> [(a,[e])]
doParse p s = runStateT p s

-- | Return the next character
getC :: GenParser e e 
getC = do
  (x:xs) <- get
  put xs
  return x

-- | Return the next character if it satisfies the given predicate
-- (this was called satP in lecture)
satisfy :: (e -> Bool) -> GenParser e e 
satisfy p = do c <- getC
               if (p c) then return c else fail "End of input"

-- | Combine two parsers together in parallel, producing all 
-- possible results from either parser.                 
choose :: GenParser e a -> GenParser e a -> GenParser e a
p1 `choose` p2 = StateT (\cs -> doParse p1 cs ++ doParse p2 cs)

-- | Combine two parsers together in parallel, but only use the 
-- first result. This means that the second parser is used only 
-- if the first parser completely fails. 
(<->) :: GenParser e a -> GenParser e a -> GenParser e a
p1 <-> p2 = StateT $ \cs -> case doParse (p1 `choose` p2) cs of
                              []  -> []
                              x:_ -> [x]

