{-# LANGUAGE GADTs #-}

module PropLogic where

import Parser
import ParserCombinators

--import Test.QuickCheck
import Test.HUnit

-- | A proposition in formal, propositional logic
data Prop where
  F   :: Prop
  Var :: Char -> Prop
  And :: Prop -> Prop -> Prop
  Or  :: Prop -> Prop -> Prop
  Imp :: Prop -> Prop -> Prop
  deriving Eq

instance Show Prop where
  show (Var c)   = [c]
  show (Imp p q) = "(" ++ (show p) ++ " => " ++ (show q) ++ ")"
  show (And p q) = "(" ++ (show p) ++ " && " ++ (show q) ++ ")"
  show (Or p q)  = "(" ++ (show p) ++ " || " ++ (show q) ++ ")"
  show F         = "!"

display :: (Show a) => a -> String
display = show

-- | Logical negation
neg :: Prop -> Prop
neg p = Imp p F

-- | Bidirectional implication
iff :: Prop -> Prop -> Prop
p `iff` q = And (Imp p q) (Imp q p)


-- Some shorcuts
(<&&>) :: Prop -> Prop -> Prop
(<&&>) = And

(<||>) :: Prop -> Prop -> Prop
(<||>) = Or

imp :: Prop -> Prop -> Prop
imp = Imp

(==>) :: Char -> Char -> Prop
p ==> q = (Var p) `imp` (Var q)

(<&>) :: Char -> Char -> Prop
p <&> q = (Var p) <&&> (Var q)

(<|>) :: Char -> Char -> Prop
p <|> q = (Var p) <||> (Var q)

(!) :: Char -> Prop
(!) p = Imp (Var p) F

wsP :: GenParser Char a -> GenParser Char a
wsP p = between whitespace p whitespace where
  whitespace = many $ choice [string " ", string "\n"]

--propP :: GenParser Prop Prop

-- | Simple Tests
p1 :: Prop
p1 = Imp (And (Var 'P') (Var 'Q')) (Var 'P')

t0 :: Test
t0 = TestList [ display p1 ~?= "((P && Q) => P)" ]

main :: IO()
main = do
  _ <- runTestTT (TestList [ t0 ])
  return ()


