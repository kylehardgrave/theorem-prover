-- Advanced Programming, Final Project
-- by Jason Mow (jmow), Kyle Hardgrave (kyleh)
{-# LANGUAGE GADTs #-}

module ProofTrees where

import           Data.List
import           Data.Maybe
import qualified Data.Set as Set
import           Text.PrettyPrint.HughesPJ (Doc, (<+>), ($$), (<>))
import           Text.PrettyPrint.HughesPJ as PP
import           PropLogic

-- | A proposition in the Gentzen sequent calculus. Comprised of 
-- assumptions and an inferred proposition.
type Seq = ([Prop], Prop)

-- | A proof tree.
data Proof = P Rule Seq [Proof]

data Rule = Axiom
          | OrL  
          | AndL1
          | AndL2
          | ImpL 
          | NegL 
          | OrR1 
          | OrR2 
          | AndR 
          | ImpR 
          | NegR
          deriving Show


class PP a where
  pp :: a -> Doc

instance PP Proof where
  pp (P r (as, b) ps)  = PP.vcat [
    (PP.text $ displayList as) <+> PP.text "|-" <+> (PP.text $ display b) <+> 
    PP.parens (PP.text $ show r),
    PP.nest 2 $ PP.vcat (map pp ps)
    ]

instance Show Proof where
  show = show . pp


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
proveAxiom s@(as, p@(Var c)) | p `elem` as = Just $ P Axiom s []
                             | otherwise   = Nothing
proveAxiom _ = Nothing

proveOrR :: Seq -> Maybe Proof
proveOrR s@(as, Exp Or p q) = case (prove (as, p), prove (as, q)) of
    (Just t, _) -> Just $ P OrR1 s [t]
    (_, Just t) -> Just $ P OrR2 s [t]
    _           -> Nothing
proveOrR _ = Nothing

proveAndR :: Seq -> Maybe Proof
proveAndR s@(as, Exp And p q) = do t1 <- prove (as, p)
                                   t2 <- prove (as, q)
                                   return $ P AndR s [t1, t2]
proveAndR _ = Nothing

proveImpR :: Seq -> Maybe Proof
proveImpR s@(as, Exp Imp p q) = prove (p:as, q) >>= return . P ImpR s . (:[])
proveImpR _ = Nothing

proveOrL :: Seq -> Maybe Proof
proveOrL s@(Exp Or p q : as, b) = do t1 <- prove (p:as, b)
                                     t2 <- prove (q:as, b)
                                     return $ P OrL s [t1, t2]
proveOrL _ = Nothing

proveAndL :: Seq -> Maybe Proof
proveAndL s@(Exp And p q : as, b) =
  case (prove (p:as, b), prove (q:as, b)) of
    (Just t, _) -> Just $ P AndL1 s [t]
    (_, Just t) -> Just $ P AndL2 s [t]
    _           -> Nothing
proveAndL _ = Nothing

proveImpL :: Seq -> Maybe Proof
proveImpL s@(Exp Imp p q : as, b) = do t1 <- prove (as, p)
                                       t2 <- prove (q:as, b)
                                       return $ P ImpL s [t1, t2]
proveImpL _ = Nothing

proveNegL :: Seq -> Maybe Proof
proveNegL s@(Exp Imp p F : as, _) = prove (as, p) >>= return . P NegL s . (:[])
proveNegL _ = Nothing

proveNegR :: Seq -> Maybe Proof
proveNegR s@(Exp Imp p F : as, _) = prove (as, p) >>= return . P NegL s . (:[])
proveNegR s@(as, Exp Imp p F) = prove (p:as, F) >>= return . P NegR s . (:[])
proveNegR _ = Nothing
