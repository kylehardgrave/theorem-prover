-- Some example propositions
import PropLogic


p = Imp (And (Var 'P') (Var 'Q')) (Var 'P')


-- | De Morgan's Laws
dm1 :: Prop
dm1 = (neg $ 'P' <&> 'Q') `iff` (((!) 'P') <||> ((!) 'Q'))
dm2 = (neg $ 'P' <|> 'Q') `iff` (((!) 'P') <&&> ((!) 'Q'))
