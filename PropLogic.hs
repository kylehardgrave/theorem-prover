{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -XFlexibleInstances #-}

module PropLogic where

--import Test.QuickCheck
import Test.HUnit

-- | A proposition in formal, propositional logic
data Prop where
  F   :: Prop
  Var :: Char -> Prop
  Exp :: Op -> Prop -> Prop -> Prop
  deriving Eq

data Op =
    And
  | Or
  | Imp
  deriving Eq

-- This is the same as:
--data Prop = F
--          | Var Char
--          | And Prop Prop
--          | Or Prop Prop
--          | Imp Prop Prop

instance Show Prop where
  show (Var c)   = [c] 
  show (Exp Imp p q) = "(" ++ (show p) ++ " => " ++ (show q) ++ ")"
  show (Exp And p q) = "(" ++ (show p) ++ " && " ++ (show q) ++ ")"
  show (Exp Or p q)  = "(" ++ (show p) ++ " || " ++ (show q) ++ ")"
  show F         = "!"

instance Show Op where
  show Imp = "(=>)"
  show And = "(&&)"
  show Or  = "(||)"  

display :: (Show a) => a -> String
display = show

-- | Logical negation
neg :: Prop -> Prop
neg p = Exp Imp p F

-- | Bidirectional implication
iff :: Prop -> Prop -> Prop
p `iff` q = Exp And (Exp Imp p q) (Exp Imp q p)


-- Some shorcuts
(<&&>) :: Prop -> Prop -> Prop
(<&&>) = Exp And

(<||>) :: Prop -> Prop -> Prop
(<||>) = Exp Or

imp :: Prop -> Prop -> Prop
imp = Exp Imp

(==>) :: Char -> Char -> Prop
p ==> q = (Var p) `imp` (Var q)

(<&>) :: Char -> Char -> Prop
p <&> q = (Var p) <&&> (Var q)

(<|>) :: Char -> Char -> Prop
p <|> q = (Var p) <||> (Var q)

(!) :: Char -> Prop
(!) p = Exp Imp (Var p) F

-- | Simple Tests
p1 :: Prop
p1 = Exp Imp (Exp And (Var 'P') (Var 'Q')) (Var 'P')
p2 :: Prop
p2 = Exp Imp (Exp Or (Var 'A') (Exp And (Var 'P') (Var 'Q'))) (Var 'P')

t0 :: Test
t0 = TestList [ display p1 ~?= "((P && Q) => P)",
                display p2 ~?= "((A || (P && Q)) => P)"]

main :: IO()
main = do
  _ <- runTestTT (TestList [ t0 ])
  return ()


