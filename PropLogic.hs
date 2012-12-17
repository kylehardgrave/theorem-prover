{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -XFlexibleInstances #-}

module PropLogic where

import qualified Data.Set as Set
import           Parser
import           ParserCombinators
import           Test.QuickCheck
import           Test.HUnit


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
  show (Exp Imp p F) = "!" ++ (show p)
  show (Exp Imp p q) = "(" ++ (show p) ++ " => " ++ (show q) ++ ")"
  show (Exp And p q) = "(" ++ (show p) ++ " && " ++ (show q) ++ ")"
  show (Exp Or p q)  = "(" ++ (show p) ++ " || " ++ (show q) ++ ")"
  show F         = "!"

-- | A given set of assumptions.
data Gam = Set.Set Prop

-- | A proof.
data Proof where
  AssumeP :: Prop  -> Proof
  ImpP    :: Proof -> Proof
  AndP    :: Proof -> Proof -> Proof
--  OrP1 :: Proof a -> Proof (Or a b)
--  OrP2 :: Proof b -> Proof (Or a b)
--  AndP :: Proof a -> Proof b -> Proof (And a b)
--  VarP :: Proof (And a b) -> Proof 


-- First pass at the proof types, sans monads or GADTs
prove :: Gam -> Prop -> Maybe Proof
prove g p | p `Set.member` g = Just $ AssumeP p
          | otherwise        = prove' p
  where prove' (Var _) = Nothing
        prove' (Imp q r) = case prove (Set.insert q g) r of
          Just t -> ImpP t p
          _      -> Nothing
        prove' (And q r) = case (prove g q, prove g r) of
          (Just t, Just u) -> AndP t u p
          _                -> Nothing
        prove' (Or q r) = case (prove g q, prove g r) of
          (Just t, _) -> OrP t p
          (_, Just t) -> OrP t p
          _           -> Nothing
        prove' -- TOOD
  
  
  @(Var c) | c `Set.member` p = Just $ VarP g
                | otherwise        = Nothing
prove g (And a b) = 


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


