{-# LANGUAGE GADTs #-}

module PropLgic where


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

