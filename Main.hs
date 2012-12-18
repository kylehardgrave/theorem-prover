-- Advanced Programming, Final Project
-- by Jason Mow (jmow), Kyle Hardgrave (kyleh)

module Main where
    import Parser
    import ParserCombinators
    import PropLogic hiding (main)
    import ProofTrees

    import Test.HUnit

    main :: IO()
    main = do
      _ <- runTestTT (TestList [ tp ])
      qc1
      return ()