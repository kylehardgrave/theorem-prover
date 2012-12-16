-- Advanced Programming, HW 5
-- by Jason Mow (jmow), Kyle Hardgrave (kyleh)

{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults  #-}

-- CIS 552, University of Pennsylvania
-- based on Parsec and ReadP parsing libraries
module ParserCombinators where

import Parser
import PropLogic hiding (main)
import Control.Monad
import Data.Char
import System.IO

import Test.HUnit

type ParseError = String

-- | Use a parser for a particular string. Note that this parser
-- combinator library doesn't support descriptive parse errors.
-- However, for compatibility with Parsec, we give this function 
-- the same type.
parse :: GenParser t a -> [t] -> Either ParseError a
parse parser ts = case (doParse parser ts) of
    []      -> Left  "No parses"
    [(a,_)] -> Right a
    _       -> Left  "Multiple parses"
    
parseFromFile :: GenParser Char a -> String -> IO (Either ParseError a)
parseFromFile parser filename = do 
  handle <- openFile filename ReadMode 
  ts <- hGetContents handle
  return $ parse parser ts
  
-- We've left the string functions as specific to Char for now.
-- | GenParsers for specific sorts of characters 
alpha, digit, upper, lower, space :: GenParser Char Char
alpha = satisfy isAlpha
digit = satisfy isDigit
upper = satisfy isUpper
lower = satisfy isLower
space = satisfy isSpace
   
-- | Parses and returns the specified character        
-- succeeds only if the input is exactly that character
char :: Char -> GenParser Char Char
char c = satisfy (c ==)

-- | Parses and returns the specified string. 
-- Succeeds only if the input is the given string
string :: String -> GenParser Char String
string = mapM char

-- | succeed only if the input is a (positive or negative) integer
int :: GenParser Char Int
int = do n <- string "-" <-> return []
         s <- many1 digit  
         return $ (read (n ++ s) :: Int)

-- | given a parser, apply it as many times as possible                         
-- and return the answer in a list
many   :: GenParser t a -> GenParser t [a]
many p = many1 p <-> many0 
   where many0 = return []
                    
-- | given a parser, apply it as many times as possible,
-- but at least once.
many1 :: GenParser t a -> GenParser t [a]
many1 p = do x  <- p
             xs <- many p
             return (x:xs)

-- | @chainl p op x@ parses zero or more occurrences of @p@, separated by @op@.
--   Returns a value produced by a /left/ associative application of all
--   functions returned by @op@. If there are no occurrences of @p@, @x@ is
--   returned.
chainl :: GenParser t b -> GenParser t (b -> b -> b) -> b -> GenParser t b
chainl p op x = chainl1 p op <-> return x

-- | Like 'chainl', but parses one or more occurrences of @p@.
chainl1 :: GenParser t a -> GenParser t (a -> a -> a) -> GenParser t a
p `chainl1` pop = p >>= rest
    where rest x = next x <-> return x 
          next x = do o <- pop
                      y <- p
                      rest $ x `o` y 
                      
                      
-- | Combine all parsers in the list (sequentially)
choice :: [GenParser t a] -> GenParser t a
choice = foldr (<->) (fail "")

-- | @between open close p@ parses @open@, followed by @p@ and finally
--   @close@. Only the value of @p@ is returned.
between :: GenParser t open -> GenParser t a -> GenParser t close -> GenParser t a
between open p close = do _ <- open
                          x <- p
                          _ <- close
                          return x

-- | @sepBy p sep@ parses zero or more occurrences of @p@, separated by @sep@.
--   Returns a list of values returned by @p@.
sepBy :: GenParser t a -> GenParser t sep -> GenParser t [a]
sepBy p sep = sepBy1 p sep <-> return []

-- | @sepBy1 p sep@ parses one or more occurrences of @p@, separated by @sep@.
--   Returns a list of values returned by @p@.
sepBy1 :: GenParser t a -> GenParser t sep -> GenParser t [a]
sepBy1 p sep = liftM2 (:) p (many (sep >> p))

wsP :: GenParser Char a -> GenParser Char a
wsP p = between whitespace p whitespace where
  whitespace = many $ choice [string " ", string "\n"]

constP :: String -> a -> GenParser Char a
constP s x = do 
  _ <- string s
  return x

fP :: GenParser Char Prop
fP = do
  c <- constP "!" F <-> fail "Not Falsity"
  return F

-- | Parses a variable, ignores following uppers
--    Does not handle lowers in input, lowers are invalid
--    See tests for definition
varP :: GenParser Char Prop
varP = do
  (x:xs) <- many1 upper
  return (Var x)

tp :: Test
tp = TestList [ t1c0, t1c1, t1c2 ]

t1c0, t1c1, t1c2 :: Test
t1c0 = doParse varP "A => B" ~?= [(Var 'A', " => B")]
-- YZ is ignored here, because it is invalid for variables to be strings
t1c1 = doParse varP "XYZ && Y => Z" ~?= [(Var 'X', " && Y => Z")]
t1c2 = doParse varP "X||Y => Z" ~?= [(Var 'X', "||Y => Z")]

main :: IO()
main = do
  _ <- runTestTT (TestList [ tp ])
  return ()

--propP :: GenParser Prop Prop
--propP F = undefined
--propP (Var a) = undefined
--propP (And p1 p2) = undefined
--propP (Or p1 p2) = undefined
--propP (Imp p1 p2) = undefine


