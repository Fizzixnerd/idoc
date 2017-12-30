module Text.IDoc.Blocks.Math where

import Text.IDoc.Syntax
import Text.IDoc.Parse
import Text.IDoc.Render.Html5.Card
import Text.IDoc.Render.Html5.Icons
import Text.IDoc.Render.Tex

import Text.LaTeX
import Text.LaTeX.Base.Class
import Text.LaTeX.Packages.AMSThm as T
import Text.LaTeX.Packages.AMSMath as M

import Text.Blaze.Html5

import Data.Data

import Control.Lens

import ClassyPrelude

data DisplayMathB = MathB { _math :: Math }
                  | EquationB { _equation :: Equation }
                  | AlignB { _align :: Align }
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

instance BlockMarkup m DisplayMathB where
  blockMarkup a_ t s (MathB m) = blockMarkup a_ t s m
  blockMarkup a_ t s (EquationB e) = blockMarkup a_ t s e
  blockMarkup a_ t s (AlignB al) = blockMarkup a_ t s al

instance Markupy m => Blocky m DisplayMathB where
  blocky a_ t s (MathB m) = blocky a_ t s m
  blocky a_ t s (EquationB e) = blocky a_ t s e
  blocky a_ t s (AlignB al) = blocky a_ t s al

data TheoremLikeB m b = TheoremB { _theorem :: Theorem m b}
                      | LemmaB { _lemma :: Lemma m b}
                      | CorollaryB { _corollary :: Corollary m b}
                      | PropositionB { _proposition :: Proposition m b}
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

instance (MarkupMarkup m, BlockMarkup m (b m)) => BlockMarkup m (TheoremLikeB m b) where
  blockMarkup a_ t s (TheoremB thm) = blockMarkup a_ t s thm
  blockMarkup a_ t s (LemmaB lem) = blockMarkup a_ t s lem
  blockMarkup a_ t s (CorollaryB cor) = blockMarkup a_ t s cor
  blockMarkup a_ t s (PropositionB prop) = blockMarkup a_ t s prop

instance (Markupy m, Blocky m (b m)) => Blocky m (TheoremLikeB m b) where
  blocky a_ t s (TheoremB thm) = blocky a_ t s thm
  blocky a_ t s (LemmaB lem) = blocky a_ t s lem
  blocky a_ t s (CorollaryB cor) = blocky a_ t s cor
  blocky a_ t s (PropositionB prop) = blocky a_ t s prop

data Math = Math { _mathContents :: Vector Token }
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

instance ToMarkup Math where
  toMarkup (Math m) = vectorBlockToMarkup "idocMath center-block flex" 
                      (\x -> "\\[" ++ x ++ "\\]") m

instance BlockMarkup m Math where
  blockMarkup _ _ _ m = toMarkup m

instance Markupy m => Blocky m Math where
  blocky _ _ msid (Math m) = mLabel msid $ mathDisplay $ raw $ concatMap unToken m

data Equation = Equation { _equationContents :: Vector Token }
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

instance ToMarkup Equation where
  toMarkup (Equation e) = vectorBlockToMarkup "idocEquation center-block flex" 
                          (\x -> "$$\\begin{equation}\n" ++ x ++ 
                                 "\\end{equation}$$") e

instance BlockMarkup m Equation where
  blockMarkup _ _ _ e = toMarkup e

instance Markupy m => Blocky m Equation where
  blocky _ _ msid (Equation e) = mLabel msid $ M.equation $ raw $ concatMap unToken e

data Align = Align { _alignContents :: Vector Token }
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

instance ToMarkup Align where
  toMarkup (Align a_) = vectorBlockToMarkup "idocAlign center-block"
                        (\x -> "$$\\begin{align}\n" ++ 
                               x ++
                               "\\end{align}$$") $ a_

instance BlockMarkup m Align where
  blockMarkup _ _ _ al = toMarkup al

instance Markupy m => Blocky m Align where
  blocky _ _ msid (Align a_) = mLabel msid $ M.align [raw $ concatMap unToken a_]

data Theorem m b = Theorem { _theoremStatement :: Vector (Core m b)
                           , _theoremProof :: Maybe (Vector (Core m b)) }
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

instance (MarkupMarkup m, BlockMarkup m (b m)) => BlockMarkup m (Theorem m b) where
  blockMarkup _ t s (Theorem thm pf) = card
                                       infoCardOptions
                                       (mTitle "Theorem" t)
                                       s
                                       (icon "fa-star-o")
                                       (vectorBlockToMarkup "idocTheoremProof" id <$> pf)
                                       (vectorBlockToMarkup "idocTheoremStatement" id thm)

data Lemma m b = Lemma { _lemmaStatement :: Vector (Core m b)
                       , _lemmaProof :: Maybe (Vector (Core m b)) }
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

instance (MarkupMarkup m, BlockMarkup m (b m)) => BlockMarkup m (Lemma m b) where
  blockMarkup _ t s (Lemma lem pf) = card
                                     infoCardOptions
                                     (mTitle "Lemma" t)
                                     s
                                     (icon "fa-star-o")
                                     (vectorBlockToMarkup "idocLemmaProof" id <$> pf)
                                     (vectorBlockToMarkup "idocLemmaStatement" id lem)

data Corollary m b = Corollary { _corollaryStatement :: Vector (Core m b)
                               , _corollaryProof :: Maybe (Vector (Core m b)) }
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

instance (MarkupMarkup m, BlockMarkup m (b m)) => BlockMarkup m (Corollary m b) where
  blockMarkup _ t s (Corollary cor pf) = card
                                         infoCardOptions
                                         (mTitle "Corollary" t)
                                         s
                                         (icon "fa-star-o")
                                         (vectorBlockToMarkup "idocCorollaryProof" id <$> pf)
                                         (vectorBlockToMarkup "idocCorollaryStatement" id cor)

data Proposition m b = Proposition { _propositionStatement :: Vector (Core m b)
                                   , _propositionProof :: Maybe (Vector (Core m b)) }
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

instance (MarkupMarkup m, BlockMarkup m (b m)) => BlockMarkup m (Proposition m b) where
  blockMarkup _ t s (Proposition prp pf) = card
                                           infoCardOptions
                                           (mTitle "Proposition" t)
                                           s
                                           (icon "fa-star-o")
                                           (vectorBlockToMarkup "idocPropositionProof" id <$> pf)
                                           (vectorBlockToMarkup "idocPropositionStatement" id prp)

data Conjecture m b = Conjecture { _conjectureContents :: Vector (Core m b) }
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

instance (MarkupMarkup m, BlockMarkup m (b m)) => BlockMarkup m (Conjecture m b) where
  blockMarkup _ t s (Conjecture c) = card
                                     infoCardOptions
                                     (mTitle "Conjecture" t)
                                     s
                                     (icon "fa-question")
                                     Nothing
                                     (vectorBlockToMarkup "idocConjecture" id c)

data Proof m b = Proof { _proofContents :: Vector (Core m b) }
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

instance (MarkupMarkup m, BlockMarkup m (b m)) => BlockMarkup m (Proof m b) where
  blockMarkup _ t s (Proof pf) = card
                                 infoCardOptions
                                 (mTitle "Proof" t)
                                 s
                                 (icon "fa-star")
                                 Nothing
                                 (vectorBlockToMarkup "idocProof" id pf)

data Axiom m b = Axiom { _axiomContents :: Vector (Core m b) }
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

instance (MarkupMarkup m, BlockMarkup m (b m)) => BlockMarkup m (Axiom m b) where
  blockMarkup _ t s (Axiom ax) = card
                                 infoCardOptions
                                 (mTitle "Axiom" t)
                                 s
                                 (icon "fa-cube")
                                 Nothing
                                 (vectorBlockToMarkup "idocAxiom" id ax)

data Definition m b = Definition { _definitionContents :: Vector (Core m b) }
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

instance (MarkupMarkup m, BlockMarkup m (b m)) => BlockMarkup m (Text.IDoc.Blocks.Math.Definition m b) where
  blockMarkup _ t s (Text.IDoc.Blocks.Math.Definition d) = card
                                                           infoCardOptions
                                                           (mTitle "Definition" t)
                                                           s
                                                           (icon "fa-cube")
                                                           Nothing
                                                           (vectorBlockToMarkup "idocDefinition" id d)

mathP :: IDocParser Math
mathP = Math <$> uninterpretedBlockP

equationP :: IDocParser Equation
equationP = Equation <$> uninterpretedBlockP

alignP :: IDocParser Align
alignP = Align <$> uninterpretedBlockP

theoremP :: MarkupParser m -> BlockParser m b -> IDocParser (Theorem m b)
theoremP m b_ = do
  (s, p_) <- coreBlockWithOptionalP m b_
  return $ Theorem s p_

lemmaP :: MarkupParser m -> BlockParser m b -> IDocParser (Lemma m b)
lemmaP m b_ = do
  (s, p_) <- coreBlockWithOptionalP m b_
  return $ Lemma s p_

corollaryP :: MarkupParser m -> BlockParser m b -> IDocParser (Corollary m b)
corollaryP m b_ = do
  (s, p_) <- coreBlockWithOptionalP m b_
  return $ Corollary s p_

propositionP :: MarkupParser m -> BlockParser m b -> IDocParser (Proposition m b)
propositionP m b_ = do
  (s, p_) <- coreBlockWithOptionalP m b_
  return $ Proposition s p_

conjectureP :: MarkupParser m -> BlockParser m b -> IDocParser (Conjecture m b)
conjectureP m b_ = Conjecture <$> coreBlockP m b_

axiomP :: MarkupParser m -> BlockParser m b -> IDocParser (Axiom m b)
axiomP m b_ = Axiom <$> coreBlockP m b_

proofP :: MarkupParser m -> BlockParser m b -> IDocParser (Proof m b)
proofP m b_ = Proof <$> coreBlockP m b_

definitionP :: MarkupParser m -> BlockParser m b -> IDocParser (Definition m b)
definitionP m b_ = Text.IDoc.Blocks.Math.Definition <$> coreBlockP m b_

theoremBlock :: (LaTeXC l, Markupy m, Blocky m (b m)) => 
                Maybe (BlockTitle m)
             -> Maybe (SetID m)
             -> Vector (Core m b)
             -> Maybe (Vector (Core m b))
             -> String
             -> l
theoremBlock _ msid thm mprf ttype = mLabel msid $
                                     T.theorem ttype $
                                     vectorTexy thm ++
                                     maybe "" (T.proof Nothing . vectorTexy) mprf

instance (Markupy m, Blocky m (b m)) => Blocky m (Theorem m b) where
  blocky _ mt msid (Theorem thm mprf) = theoremBlock mt msid thm mprf "Theorem"

instance (Markupy m, Blocky m (b m)) => Blocky m (Lemma m b) where
  blocky _ mt msid (Lemma thm mprf) = theoremBlock mt msid thm mprf "Lemma"

instance (Markupy m, Blocky m (b m)) => Blocky m (Corollary m b) where
  blocky _ mt msid (Corollary thm mprf) = theoremBlock mt msid thm mprf "Corollary"

instance (Markupy m, Blocky m (b m)) => Blocky m (Proposition m b) where
  blocky _ mt msid (Proposition thm mprf) = theoremBlock mt msid thm mprf "Proposition"

instance (Markupy m, Blocky m (b m)) => Blocky m (Conjecture m b) where
  blocky _ _ msid (Conjecture c) = mLabel msid $
                                   T.theorem "Conjecture" $
                                   vectorTexy c

instance (Markupy m, Blocky m (b m)) => Blocky m (Axiom m b) where
  blocky _ _ msid (Axiom a_) = mLabel msid $
                               T.theorem "Axiom" $
                               vectorTexy a_

instance (Markupy m, Blocky m (b m)) => Blocky m (Proof m b) where
  blocky _ mt msid (Proof p_) = mLabel msid $
                                T.proof (texy <$> mt) $
                                vectorTexy p_

instance (Markupy m, Blocky m (b m)) => Blocky m (Text.IDoc.Blocks.Math.Definition m b) where
  blocky _ _ msid (Text.IDoc.Blocks.Math.Definition d) = mLabel msid $
                                                         T.theorem "Definition" $
                                                         vectorTexy d

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

