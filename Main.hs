-- Advanced Programming, Final Project
-- by Jason Mow (jmow), Kyle Hardgrave (kyleh)

module Main where
    import Parser
    import ParserCombinators
    import PropLogic
    import ProofTrees

    import Test.HUnit

    main :: IO()
    main = do
      _ <- runTestTT (TestList [ t0, tp ])
      qc1
      return ()