A formal-logic theorem prover in Haskell. Final project for CIS-552.

Jason Mow & Kyle Hardgrave ({jmow,kyleh}@seas)

The main components of the project include a Propositional Logic Data definitions, a Parser for these logical statements, and a Prover for logical statements.

The propositional logic definitions are in PropLogic.hs. This class also contains utility functions to easily create Propositions from characters or other propositions using AND, OR, and IMPLIES operators.

The Parser is defined in Parser.hs and ParserCombinators.hs. These are from previous homeworks in CIS 552 and were extended to include functionality to parse propositional logic statements.

The Prover is defined in ProofTrees.hs. Here we can generate the proof trees for propositional statements based on certain assumptions, or no assumptions.
