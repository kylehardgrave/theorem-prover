{-# LANGUAGE GADTs #-}

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


