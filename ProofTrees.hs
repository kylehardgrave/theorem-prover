{-# LANGUAGE GADTs #-}

module ProofTrees where

import           Data.List
import           Data.Maybe
import qualified Data.Set as Set
import           PropLogic

-- | A proof.
--data Proof where
--  AssumeP :: Prop  -> Proof
--  ImpP    :: Proof -> Proof
--  AndP    :: Proof -> Proof -> Proof
--  OrP1 :: Proof a -> Proof (Or a b)
--  OrP2 :: Proof b -> Proof (Or a b)
--  AndP :: Proof a -> Proof b -> Proof (And a b)
--  VarP :: Proof (And a b) -> Proof

--data Proof = Axiom | P Rule Seq [Proof]
--data Sequent = Seq [Prop] [Prop]
{-
data Rule = Atom | Cut -- Identity rules
          | AndL1 | AndL2 | AndR
          | OrL | OrR1 | OrR2
          | ImpL | ImpR
          | NegL | NegR
          | WL | WR
          | CL | CR
          | PL | PR-}

type Seq = ([Prop], Prop)

data Proof where
  Axiom :: Seq -> Proof 
--  Cut   :: Seq -> Proof -> Proof -> Proof
  OrL   :: Seq -> Proof -> Proof -> Proof
  AndL1 :: Seq -> Proof -> Proof
  AndL2 :: Seq -> Proof -> Proof
  ImpL  :: Seq -> Proof -> Proof -> Proof
  NegL  :: Seq -> Proof -> Proof
  OrR1  :: Seq -> Proof -> Proof
  OrR2  :: Seq -> Proof -> Proof
  AndR  :: Seq -> Proof -> Proof -> Proof
  ImpR  :: Seq -> Proof -> Proof
  NegR  :: Seq -> Proof -> Proof
  deriving Show


--instance Show Prof where
--  show 

prove :: Seq -> Maybe Proof
prove s = case allProofs s of
  (p:_) -> p
  []    -> Nothing

allProofs s@(as, bs) = [p | a <- as
                        , rule <- rules
                        , p <- [rule (a : delete a as, bs)], isJust p] 
                     ++ [p | rule <- rules, p <- [rule s], isJust p]

rules :: [Seq -> Maybe Proof]
rules = [proveAxiom, proveOrR, proveAndR, proveImpR, proveNegR,
         proveOrL, proveAndL, proveImpL, {-proveAtomicL,-} proveNegL]

proveAxiom :: Seq -> Maybe Proof
proveAxiom s@(as, p@(Var c)) | p `elem` as = Just $ Axiom s
                             | otherwise   = Nothing
proveAxiom _ = Nothing

proveOrR :: Seq -> Maybe Proof
proveOrR s@(as, Exp Or p q) = case (prove (as, p), prove (as, q)) of
    (Just t, _) -> Just $ OrR1 s t
    (_, Just t) -> Just $ OrR2 s t
    _           -> Nothing
proveOrR _ = Nothing

proveAndR :: Seq -> Maybe Proof
proveAndR s@(as, Exp And p q) = do t1 <- prove (as, p)
                                   t2 <- prove (as, q)
                                   return $ AndR s t1 t2
proveAndR _ = Nothing

proveImpR :: Seq -> Maybe Proof
proveImpR s@(as, Exp Imp p q) = prove (p:as, q) >>= return . ImpR s
proveImpR _ = Nothing

proveOrL :: Seq -> Maybe Proof
proveOrL s@(Exp Or p q : as, b) = do t1 <- prove (p:as, b)
                                     t2 <- prove (q:as, b)
                                     return $ OrL s t1 t2
proveOrL _ = Nothing

proveAndL :: Seq -> Maybe Proof
proveAndL s@(Exp And p q : as, b) =
  case (prove (p:as, b), prove (q:as, b)) of
    (Just t, _) -> Just $ AndL1 s t
    (_, Just t) -> Just $ AndL2 s t
    _           -> Nothing
proveAndL _ = Nothing

proveImpL :: Seq -> Maybe Proof
proveImpL s@(Exp Imp p q : as, b) = do t1 <- prove (as, p)
                                       t2 <- prove (q:as, b)
                                       return $ ImpL s t1 t2
proveImpL _ = Nothing

proveNegL :: Seq -> Maybe Proof
proveNegL s@(Exp Imp p F : as, _) = prove (as, p) >>= return . NegL s
proveNegL _ = Nothing

proveNegR :: Seq -> Maybe Proof
proveNegR s@(Exp Imp p F : as, _) = prove (as, p) >>= return . NegL s
proveNegR s@(as, Exp Imp p F) = prove (p:as, F) >>= return . NegR s
proveNegR _ = Nothing

{-proveAtomicL :: Seq -> Maybe Proof
proveAtomicL (Var c : as, b) = prove (as, b)
proveAtomicL (F : as, b) = prove (as, b)
proveAtomicL _ = Nothing-}


{-prove (Var c:as) bs = prove Seq as bs
prove (Imp a F : as) bs = do p <- prove as (a:bs)
                             case p of
                               P
                             return P NegL-}

--prove :: Seq -> Maybe Proof
--prove

--prove (Seq as (Var c)) = P Atom Axiom
--prove (Seq as (Imp p

{-
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
        prove' 
  
  
  @(Var c) | c `Set.member` p = Just $ VarP g
                | otherwise        = Nothing
prove g (And a b) = 
-}


