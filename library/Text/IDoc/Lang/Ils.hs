{-# LANGUAGE DataKinds #-}

module Text.IDoc.Lang.Ils where

import ClassyPrelude hiding (Identity)

import Control.Lens hiding (Identity)

import Data.Vinyl.CoRec
import Data.Vinyl.Functor
import Data.Vinyl.Lens
import Data.Vinyl.TypeLevel

import Text.IDoc.Syntax
import Text.IDoc.Parse
import Text.IDoc.Blocks.Admonition
import Text.IDoc.Blocks.BibTex
import Text.IDoc.Blocks.Code
import Text.IDoc.Blocks.Connection
import Text.IDoc.Blocks.Example
import Text.IDoc.Blocks.Exercise
import Text.IDoc.Blocks.FurtherReading
import Text.IDoc.Blocks.IntroOutro
import Text.IDoc.Blocks.Intuition
import Text.IDoc.Blocks.Math
import Text.IDoc.Blocks.Media
import Text.IDoc.Blocks.Prerex
import Text.IDoc.Blocks.Quote
import Text.IDoc.Blocks.Recall
import Text.IDoc.Markup.Footnote
import Text.IDoc.Render.Tex

import Text.LaTeX

import Control.Monad.Fix

type IlsBlocks m = '[ AdmonitionB m BlockType
                    , BibTex
                    , Code
                    , Connection m BlockType
                    , Example m BlockType
                    , Exercise m BlockType
                    , FurtherReading m BlockType
                    , Introduction m BlockType
                    , Summary m BlockType
                    , Intuition m BlockType
                    , DisplayMathB
                    , TheoremLikeB m BlockType
                    , Conjecture m BlockType
                    , Definition m BlockType
                    , Proof m BlockType
                    , Axiom m BlockType
                    , SimpleMediaB m
                    , YouTube m
                    , Prerex m
                    , Quote m
                    , Recall m BlockType ]

newtype BlockType m = BlockType { _unBlockType :: CoRec Identity (IlsBlocks m) }
  deriving (Eq, Show)

instance BlockMarkup MarkupType (BlockType MarkupType) where
  blockMarkup attrs title_ sid (BlockType coRec) = blockMarkup attrs title_ sid coRec

instance Blocky MarkupType (BlockType MarkupType) where
  blocky attrs title_ sid (BlockType coRec) = blocky attrs title_ sid coRec

type IlsMarkup = '[ Footnote MarkupType ]

newtype MarkupType = MarkupType { _unMarkupType :: CoRec Identity IlsMarkup }
  deriving (Eq, Show)

instance MarkupMarkup MarkupType where
  markupMarkup attrs sid (MarkupType coRec) = markupMarkup attrs sid coRec

instance Markupy MarkupType where
  markupy attrs sid (MarkupType coRec) = markupy attrs sid coRec

type IlsDoc = Doc MarkupType BlockType

mkBlockType :: (Functor f, RElem a (IlsBlocks m) (RIndex a (IlsBlocks m))) => f a -> f (BlockType m)
mkBlockType = (BlockType <$> CoRec <$> Identity <$>)

mkMarkupType :: (Functor f, RElem a IlsMarkup (RIndex a IlsMarkup)) => f a -> f MarkupType
mkMarkupType = (MarkupType <$> CoRec <$> Identity <$>)

compileIls :: Text -> IlsDoc
compileIls t = runIdentity $ compileIdoc (fix markupTypeP) (fix (\b_ m_ n -> blockTypeP m_ b_ n)) t

compileIdocTexFile :: (Markupy m, Blocky m (b m), MonadIO n) => Doc m b -> FilePath -> n ()
compileIdocTexFile doc_ outFile = liftIO $ renderFile outFile $ defaultDecorator (concatMap texy $ unDocTitle $ doc_^.docTitle) (texy doc_ :: LaTeX)

blockTypeP :: MarkupParser m -> BlockParser m BlockType -> BlockTypeName -> IDocParser (BlockType m)
blockTypeP m b "info" = mkBlockType $ InfoB <$> infoP m b
blockTypeP m b "tip" = mkBlockType $ TipB <$> tipP m b
blockTypeP m b "caution" = mkBlockType $ CautionB <$> cautionP m b
blockTypeP m b "warning" = mkBlockType $ WarningB <$> warningP m b
blockTypeP m b "sidenote" = mkBlockType $ SideNoteB <$> sideNoteP m b
blockTypeP _ _ "code" = mkBlockType codeP
blockTypeP m b "connection" = mkBlockType $ connectionP m b
blockTypeP m b "example" = mkBlockType $ exampleP m b
blockTypeP m b "exercise" = mkBlockType $ exerciseP m b
blockTypeP m b "furtherreading" = mkBlockType $ furtherReadingP m b
blockTypeP m b "introduction" = mkBlockType $ introductionP m b
blockTypeP m b "summary" = mkBlockType $ summaryP m b
blockTypeP m b "intuition" = mkBlockType $ intuitionP m b
blockTypeP _ _ "math" = mkBlockType $ MathB <$> mathP
blockTypeP _ _ "equation" = mkBlockType $ EquationB <$> equationP
blockTypeP _ _ "align" = mkBlockType $ AlignB <$> alignP
blockTypeP m b "theorem" = mkBlockType $ TheoremB <$> theoremP m b
blockTypeP m b "lemma" = mkBlockType $ LemmaB <$> lemmaP m b
blockTypeP m b "corollary" = mkBlockType $ CorollaryB <$> corollaryP m b
blockTypeP m b "proposition" = mkBlockType $ PropositionB <$> propositionP m b
blockTypeP m b "conjecture" = mkBlockType $ conjectureP m b
blockTypeP m b "definition" = mkBlockType $ definitionP m b
blockTypeP m b "proof" = mkBlockType $ proofP m b
blockTypeP m b "axiom" = mkBlockType $ axiomP m b
blockTypeP m _ "image" = mkBlockType $ ImageB <$> imageP m
blockTypeP m _ "video" = mkBlockType $ VideoB <$> videoP m
blockTypeP m _ "youtube" = mkBlockType $ youTubeP m
blockTypeP m _ "prerex" = mkBlockType $ prerexP m
blockTypeP m _ "quote" = mkBlockType $ quoteP m
blockTypeP m b "recall" = mkBlockType $ recallP m b
blockTypeP _ _ s = fail $ unpack $ "Did not recognize block type: " ++ s

markupTypeP :: MarkupParser MarkupType -> MarkupTypeName -> IDocParser MarkupType
markupTypeP m "footnote" = mkMarkupType $ footnoteP m
markupTypeP _ s = fail $ unpack $ "Did not recognize markup type: " ++ s

makeLenses ''BlockType
makeLenses ''MarkupType
