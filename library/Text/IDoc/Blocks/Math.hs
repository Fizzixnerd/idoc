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

instance CheckLinks m b DisplayMathB where
  checkLinks _ _ _ = mempty

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

instance (CheckLinks m b m, CheckLinks m b (b m)) => CheckLinks m b (TheoremLikeB m b) where
  checkLinks constraints container (TheoremB thm) = checkLinks constraints container thm
  checkLinks constraints container (LemmaB lem) = checkLinks constraints container lem
  checkLinks constraints container (CorollaryB cor) = checkLinks constraints container cor
  checkLinks constraints container (PropositionB prop) = checkLinks constraints container prop

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

data Theorem m b = Theorem { _theoremStatement :: Vector (Core m b) }
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

instance (MarkupMarkup m, BlockMarkup m (b m)) => BlockMarkup m (Theorem m b) where
  blockMarkup _ t s (Theorem thm) = card
                                    infoCardOptions
                                    (mTitle "Theorem" t)
                                    s
                                    (icon "fa-star-o")
                                    Nothing
                                    (vectorBlockToMarkup "idocTheoremStatement" id thm)

instance (CheckLinks m b m, CheckLinks m b (b m)) => CheckLinks m b (Theorem m b) where
  checkLinks constraints container (Theorem ts) =
    concatMap (checkLinks constraints container) ts

data Lemma m b = Lemma { _lemmaStatement :: Vector (Core m b) }
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

instance (MarkupMarkup m, BlockMarkup m (b m)) => BlockMarkup m (Lemma m b) where
  blockMarkup _ t s (Lemma lem) = card
                                  infoCardOptions
                                  (mTitle "Lemma" t)
                                  s
                                  (icon "fa-star-o")
                                  Nothing
                                  (vectorBlockToMarkup "idocLemmaStatement" id lem)

instance (CheckLinks m b m, CheckLinks m b (b m)) => CheckLinks m b (Lemma m b) where
  checkLinks constraints container (Lemma ls) =
    concatMap (checkLinks constraints container) ls

data Corollary m b = Corollary { _corollaryStatement :: Vector (Core m b) }
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

instance (MarkupMarkup m, BlockMarkup m (b m)) => BlockMarkup m (Corollary m b) where
  blockMarkup _ t s (Corollary cor) = card
                                      infoCardOptions
                                      (mTitle "Corollary" t)
                                      s
                                      (icon "fa-star-o")
                                      Nothing
                                      (vectorBlockToMarkup "idocCorollaryStatement" id cor)

instance (CheckLinks m b m, CheckLinks m b (b m)) => CheckLinks m b (Corollary m b) where
  checkLinks constraints container (Corollary cs) =
    concatMap (checkLinks constraints container) cs

data Proposition m b = Proposition { _propositionStatement :: Vector (Core m b) }
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

instance (MarkupMarkup m, BlockMarkup m (b m)) => BlockMarkup m (Proposition m b) where
  blockMarkup _ t s (Proposition prp) = card
                                        infoCardOptions
                                        (mTitle "Proposition" t)
                                        s
                                        (icon "fa-star-o")
                                        Nothing
                                        (vectorBlockToMarkup "idocPropositionStatement" id prp)

instance (CheckLinks m b m, CheckLinks m b (b m)) => CheckLinks m b (Proposition m b) where
  checkLinks constraints container (Proposition ps) =
    concatMap (checkLinks constraints container) ps

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

instance (CheckLinks m b m, CheckLinks m b (b m)) => CheckLinks m b (Conjecture m b) where
  checkLinks constraints container (Conjecture cs) =
    concatMap (checkLinks constraints container) cs

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

instance (CheckLinks m b m, CheckLinks m b (b m)) => CheckLinks m b (Proof m b) where
  checkLinks constraints container (Proof ps) =
    concatMap (checkLinks constraints container) ps

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

instance (CheckLinks m b m, CheckLinks m b (b m)) => CheckLinks m b (Axiom m b) where
  checkLinks constraints container (Axiom as) =
    concatMap (checkLinks constraints container) as

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

instance (CheckLinks m b m, CheckLinks m b (b m)) => CheckLinks m b (Text.IDoc.Blocks.Math.Definition m b) where
  checkLinks constraints container (Text.IDoc.Blocks.Math.Definition ds) =
    concatMap (checkLinks constraints container) ds

mathP :: IDocParser m b Math
mathP = Math <$> uninterpretedBlockP

equationP :: IDocParser m b Equation
equationP = Equation <$> uninterpretedBlockP

alignP :: IDocParser m b Align
alignP = Align <$> uninterpretedBlockP

theoremP :: IDocParser m b (Theorem m b)
theoremP = Theorem <$> coreBlockP

lemmaP :: IDocParser m b (Lemma m b)
lemmaP = Lemma <$> coreBlockP

corollaryP :: IDocParser m b (Corollary m b)
corollaryP = Corollary <$> coreBlockP

propositionP :: IDocParser m b (Proposition m b)
propositionP = Proposition <$> coreBlockP

conjectureP :: IDocParser m b (Conjecture m b)
conjectureP = Conjecture <$> coreBlockP

axiomP :: IDocParser m b (Axiom m b)
axiomP = Axiom <$> coreBlockP

proofP :: IDocParser m b (Proof m b)
proofP = Proof <$> coreBlockP

definitionP :: IDocParser m b (Definition m b)
definitionP = Text.IDoc.Blocks.Math.Definition <$> coreBlockP

theoremBlock :: (LaTeXC l, Markupy m, Blocky m (b m)) => 
                Maybe (BlockTitle m)
             -> Maybe (SetID m)
             -> Vector (Core m b)
             -> String
             -> l
theoremBlock _ msid thm ttype = mLabel msid $
                                T.theorem ttype $
                                vectorTexy thm

instance (Markupy m, Blocky m (b m)) => Blocky m (Theorem m b) where
  blocky _ mt msid (Theorem thm) = theoremBlock mt msid thm "Theorem"

instance (Markupy m, Blocky m (b m)) => Blocky m (Lemma m b) where
  blocky _ mt msid (Lemma thm) = theoremBlock mt msid thm "Lemma"

instance (Markupy m, Blocky m (b m)) => Blocky m (Corollary m b) where
  blocky _ mt msid (Corollary thm) = theoremBlock mt msid thm "Corollary"

instance (Markupy m, Blocky m (b m)) => Blocky m (Proposition m b) where
  blocky _ mt msid (Proposition thm) = theoremBlock mt msid thm "Proposition"

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

