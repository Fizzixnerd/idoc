module Text.IDoc.Blocks.Math where

import Text.IDoc.Syntax

import Data.Data

import Control.Lens

import ClassyPrelude

data DisplayMathB = MathB { _math :: Math }
                  | EquationB { _equation :: Equation }
                  | AlignB { _align :: Align }
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

data TheoremLikeB a = TheoremB { _theorem :: Theorem a}
                    | LemmaB { _lemma :: Lemma a}
                    | CorollaryB { _corollary :: Corollary a}
                    | PropositionB { _proposition :: Proposition a}
                    | ConjectureB { _conjecture :: Conjecture a}
                    | ProofB { _proof :: Proof a }
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

data DefinitionLikeB a = AxiomB { _axiom :: Axiom a }
                       | DefinitionB { _definition :: Definition a }
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

data Math = Math { _mathContents :: Vector Token }
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

data Equation = Equation { _equationContents :: Vector Token }
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

data Align = Align { _alignContents :: Vector Token }
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

data Theorem a = Theorem { _theoremContents :: (Vector (Core a), Maybe (Vector (Core a))) }
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

data Lemma a = Lemma { _lemmaContents :: (Vector (Core a), Maybe (Vector (Core a))) }
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

data Corollary a = Corollary { _corollaryContents :: (Vector (Core a), Maybe (Vector (Core a))) }
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

data Proposition a = Proposition { _propositionContents :: (Vector (Core a), Maybe (Vector (Core a))) }
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

data Conjecture a = Conjecture { _conjectureContents :: Vector (Core a) }
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

data Proof a = Proof { _proofContents :: Vector (Core a) }
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

data Axiom a = Axiom { _axiomContents :: Vector (Core a) }
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

data Definition a = Definition { _definitionContents :: Vector (Core a) }
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

makeLenses ''DisplayMathB
makeLenses ''TheoremLikeB
makeLenses ''DefinitionLikeB

makeLenses ''Math
makeLenses ''Equation
makeLenses ''Align

makeLenses ''Theorem
makeLenses ''Lemma
makeLenses ''Corollary
makeLenses ''Proposition
makeLenses ''Conjecture
makeLenses ''Proof

makeLenses ''Axiom
makeLenses ''Definition
