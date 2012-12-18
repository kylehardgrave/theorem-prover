{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -XFlexibleInstances #-}

module PropLogic where

import Test.HUnit

-- | A proposition in formal, propositional logic
data Prop where
  F   :: Prop
  Var :: Char -> Prop
  Exp :: Op -> Prop -> Prop -> Prop

data Op =
    And
  | Or
  | Imp
  deriving Eq

-- We define equality of Props to account for commutativity
instance Eq Prop where
  (==) F               F               = True
  (==) (Var c1)        (Var c2)        = c1 == c2
  (==) (Exp Imp p1 q1) (Exp Imp p2 q2) = p1 == p2 && q1 == q2
  (==) (Exp And p1 q1) (Exp And p2 q2) = (p1 == p2 && q1 == q2) ||
                                         (p1 == q2 && q1 == p2)
  (==) (Exp Or p1 q1)  (Exp Or p2 q2)  = (p1 == p2 && q1 == q2) ||
                                         (p1 == q2 && q1 == p2)
  (==) _ _                             = False

-- | Pretty Print
--instance Show Prop where
--  show (Var c)   = [c] 
--  show (Exp Imp p F) = "!" ++ (show p)
--  show (Exp Imp p q) = "(" ++ (show p) ++ " => " ++ (show q) ++ ")"
--  show (Exp And p q) = "(" ++ (show p) ++ " && " ++ (show q) ++ ")"
--  show (Exp Or p q)  = "(" ++ (show p) ++ " || " ++ (show q) ++ ")"
--  show F         = "!"

-- | Show for ProofTree Prover
instance Show Prop where
  show (Var c)   = "(Var '" ++ [c] ++ "')"
  show (Exp Imp p F) = "(neg" ++ (show p) ++ ")"
  show (Exp Imp p q) = "(" ++ (show p) ++ " `imp` " ++ (show q) ++ ")"
  show (Exp And p q) = "(" ++ (show p) ++ " <&&> " ++ (show q) ++ ")"
  show (Exp Or p q)  = "(" ++ (show p) ++ " <||> " ++ (show q) ++ ")"
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

-- Simple Tests
prop1 :: Prop
prop1 = Exp Imp (Exp And (Var 'P') (Var 'Q')) (Var 'P')
prop2 :: Prop
prop2 = Exp Imp (Exp Or (Var 'A') (Exp And (Var 'P') (Var 'Q'))) (Var 'P')

t0 :: Test
t0 = TestList [ display prop1 ~?= "((P && Q) => P)",
                display prop2 ~?= "((A || (P && Q)) => P)"]

main :: IO()
main = do
  _ <- runTestTT (TestList [ t0 ])
  return ()


