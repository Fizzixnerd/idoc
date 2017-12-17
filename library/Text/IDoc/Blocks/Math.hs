module Text.IDoc.Blocks.Math where

import Text.IDoc.Syntax
import Text.IDoc.Parse
import Text.IDoc.Render.Html5.Card
import Text.IDoc.Render.Html5.Icons

import Text.Blaze.Html5

import Data.Data

import Control.Lens

import ClassyPrelude

data DisplayMathB = MathB { _math :: Math }
                  | EquationB { _equation :: Equation }
                  | AlignB { _align :: Align }
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

instance BlockMarkup DisplayMathB where
  blockMarkup a_ t s (MathB m) = blockMarkup a_ t s m
  blockMarkup a_ t s (EquationB e) = blockMarkup a_ t s e
  blockMarkup a_ t s (AlignB al) = blockMarkup a_ t s al

data TheoremLikeB a = TheoremB { _theorem :: Theorem a}
                    | LemmaB { _lemma :: Lemma a}
                    | CorollaryB { _corollary :: Corollary a}
                    | PropositionB { _proposition :: Proposition a}
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

instance BlockMarkup a => BlockMarkup (TheoremLikeB a) where
  blockMarkup a_ t s (TheoremB thm) = blockMarkup a_ t s thm
  blockMarkup a_ t s (LemmaB lem) = blockMarkup a_ t s lem
  blockMarkup a_ t s (CorollaryB cor) = blockMarkup a_ t s cor
  blockMarkup a_ t s (PropositionB prop) = blockMarkup a_ t s prop

data Math = Math { _mathContents :: Vector Token }
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

instance ToMarkup Math where
  toMarkup (Math m) = vectorBlockToMarkup "idocMath center-block flex" 
                      (\x -> "\\[" ++ x ++ "\\]") m

instance BlockMarkup Math where
  blockMarkup _ _ _ m = toMarkup m

data Equation = Equation { _equationContents :: Vector Token }
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

instance ToMarkup Equation where
  toMarkup (Equation e) = vectorBlockToMarkup "idocEquation center-block flex" 
                          (\x -> "$$\\begin{equation}\n" ++ x ++ 
                                 "\\end{equation}$$") e

instance BlockMarkup Equation where
  blockMarkup _ _ _ e = toMarkup e

data Align = Align { _alignContents :: Vector Token }
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

instance ToMarkup Align where
  toMarkup (Align a_) = vectorBlockToMarkup "idocAlign center-block"
                       (\x -> "$$\\begin{align}\n" ++ 
                              x ++
                              "\\end{align}$$") $ a_

instance BlockMarkup Align where
  blockMarkup _ _ _ al = toMarkup al

data Theorem a = Theorem { _theoremStatement :: Vector (Core a)
                         , _theoremProof :: Maybe (Vector (Core a)) }
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

instance BlockMarkup a => BlockMarkup (Theorem a) where
  blockMarkup _ t s (Theorem thm pf) = card
                                       infoCardOptions
                                       (mTitle "Theorem" t)
                                       s
                                       (icon "fa-star-o")
                                       (vectorBlockToMarkup "idocTheoremProof" id <$> pf)
                                       (vectorBlockToMarkup "idocTheoremStatement" id thm)

data Lemma a = Lemma { _lemmaStatement :: Vector (Core a)
                     , _lemmaProof :: Maybe (Vector (Core a)) }
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

instance BlockMarkup a => BlockMarkup (Lemma a) where
  blockMarkup _ t s (Lemma lem pf) = card
                                     infoCardOptions
                                     (mTitle "Lemma" t)
                                     s
                                     (icon "fa-star-o")
                                     (vectorBlockToMarkup "idocLemmaProof" id <$> pf)
                                     (vectorBlockToMarkup "idocLemmaStatement" id lem)

data Corollary a = Corollary { _corollaryStatement :: Vector (Core a)
                             , _corollaryProof :: Maybe (Vector (Core a)) }
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

instance BlockMarkup a => BlockMarkup (Corollary a) where
  blockMarkup _ t s (Corollary cor pf) = card
                                         infoCardOptions
                                         (mTitle "Corollary" t)
                                         s
                                         (icon "fa-star-o")
                                         (vectorBlockToMarkup "idocCorollaryProof" id <$> pf)
                                         (vectorBlockToMarkup "idocCorollaryStatement" id cor)

data Proposition a = Proposition { _propositionStatement :: Vector (Core a)
                                 , _propositionProof :: Maybe (Vector (Core a)) }
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

instance BlockMarkup a => BlockMarkup (Proposition a) where
  blockMarkup _ t s (Proposition prp pf) = card
                                           infoCardOptions
                                           (mTitle "Proposition" t)
                                           s
                                           (icon "fa-star-o")
                                           (vectorBlockToMarkup "idocPropositionProof" id <$> pf)
                                           (vectorBlockToMarkup "idocPropositionStatement" id prp)

data Conjecture a = Conjecture { _conjectureContents :: Vector (Core a) }
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

instance BlockMarkup a => BlockMarkup (Conjecture a) where
  blockMarkup _ t s cjc = card
                          infoCardOptions
                          (mTitle "Conjecture" t)
                          s
                          (icon "fa-question")
                          Nothing
                          (toMarkup cjc)

instance BlockMarkup a => ToMarkup (Conjecture a) where
  toMarkup (Conjecture c) = vectorBlockToMarkup "idocConjecture" id c

data Proof a = Proof { _proofContents :: Vector (Core a) }
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

instance BlockMarkup a => ToMarkup (Proof a) where
  toMarkup (Proof p_) = vectorBlockToMarkup "idocProof" id p_

instance BlockMarkup a => BlockMarkup (Proof a) where
  blockMarkup _ t s pf = card
                         infoCardOptions
                         (mTitle "Proof" t)
                         s
                         (icon "fa-star")
                         Nothing
                         (toMarkup pf)

data Axiom a = Axiom { _axiomContents :: Vector (Core a) }
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

instance BlockMarkup a => ToMarkup (Axiom a) where
  toMarkup (Axiom a_) = vectorBlockToMarkup "idocAxiom" id a_

instance BlockMarkup a => BlockMarkup (Axiom a) where
  blockMarkup _ t s ax = card
                         infoCardOptions
                         (mTitle "Axiom" t)
                         s
                         (icon "fa-cube")
                         Nothing
                         (toMarkup ax)

data Definition a = Definition { _definitionContents :: Vector (Core a) }
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

instance BlockMarkup a => ToMarkup (Definition a) where
  toMarkup (Definition d) = vectorBlockToMarkup "idocDefinition" id d

instance BlockMarkup a => BlockMarkup (Definition a) where
  blockMarkup _ t s def = card
                          infoCardOptions
                          (mTitle "Definition" t)
                          s
                          (icon "fa-cube")
                          Nothing
                          (toMarkup def)

mathP :: IDocParser Math
mathP = Math <$> uninterpretedBlockP

equationP :: IDocParser Equation
equationP = Equation <$> uninterpretedBlockP

alignP :: IDocParser Align
alignP = Align <$> uninterpretedBlockP

theoremP :: BlockParser a -> IDocParser (Theorem a)
theoremP b_ = do
  (s, p_) <- coreBlockWithOptionalP b_
  return $ Theorem s p_

lemmaP :: BlockParser a -> IDocParser (Lemma a)
lemmaP b_ = do
  (s, p_) <- coreBlockWithOptionalP b_
  return $ Lemma s p_

corollaryP :: BlockParser a -> IDocParser (Corollary a)
corollaryP b_ = do
  (s, p_) <- coreBlockWithOptionalP b_
  return $ Corollary s p_

propositionP :: BlockParser a -> IDocParser (Proposition a)
propositionP b_ = do
  (s, p_) <- coreBlockWithOptionalP b_
  return $ Proposition s p_

conjectureP :: BlockParser a -> IDocParser (Conjecture a)
conjectureP b_ = Conjecture <$> coreBlockP b_

axiomP :: BlockParser a -> IDocParser (Axiom a)
axiomP b_ = Axiom <$> coreBlockP b_

proofP :: BlockParser a -> IDocParser (Proof a)
proofP b_ = Proof <$> coreBlockP b_

definitionP :: BlockParser a -> IDocParser (Definition a)
definitionP b_ = Definition <$> coreBlockP b_

makeLenses ''DisplayMathB
makeLenses ''TheoremLikeB

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
