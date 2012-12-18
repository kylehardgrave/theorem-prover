-- Advanced Programming, Final Project
-- by Jason Mow (jmow), Kyle Hardgrave (kyleh)

-- Some example propositions
module Props where

import PropLogic


-- | De Morgan's Laws
dm1 :: Prop
dm1 = (neg $ 'P' <&> 'Q') `iff` (((!) 'P') <||> ((!) 'Q'))
dm2 = (neg $ 'P' <|> 'Q') `iff` (((!) 'P') <&&> ((!) 'Q'))
