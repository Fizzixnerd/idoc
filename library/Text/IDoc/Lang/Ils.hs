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
import Text.IDoc.Render.Tex

import Text.LaTeX

type IlsBlocks = '[ AdmonitionB BlockType
                  , BibTex
                  , Code
                  , Connection BlockType
                  , Example BlockType
                  , Exercise BlockType
                  , FurtherReading BlockType
                  , Introduction BlockType
                  , Summary BlockType
                  , Intuition BlockType
                  , DisplayMathB
                  , TheoremLikeB BlockType
                  , Conjecture BlockType
                  , Definition BlockType
                  , Proof BlockType
                  , Axiom BlockType
                  , SimpleMediaB
                  , YouTube
                  , Prerex
                  , Quote
                  , Recall BlockType ]

newtype BlockType = BlockType { _unBlockType :: CoRec Identity IlsBlocks }
  deriving (Eq, Show)

instance BlockMarkup BlockType where
  blockMarkup attrs title_ sid (BlockType coRec) = blockMarkup attrs title_ sid coRec

instance Blocky BlockType where
  block attrs title_ sid (BlockType coRec) = block attrs title_ sid coRec

mkBlockType :: (Functor f, RElem a IlsBlocks (RIndex a IlsBlocks)) => f a -> f BlockType
mkBlockType = (BlockType <$> CoRec <$> Identity <$>)

compileIdocTexFile :: (Blocky a, MonadIO m) => (Doc a) -> FilePath -> m ()
compileIdocTexFile doc_ outFile = liftIO $ renderFile outFile $ defaultDecorator (concatMap texy $ unDocTitle $ doc_^.docTitle) (texy doc_ :: LaTeX)

type IlsDoc = Doc BlockType

blockTypeP :: BlockParser BlockType -> BlockTypeName -> IDocParser BlockType
blockTypeP b "info" = mkBlockType $ InfoB <$> infoP b
blockTypeP b "tip" = mkBlockType $ TipB <$> tipP b
blockTypeP b "caution" = mkBlockType $ CautionB <$> cautionP b
blockTypeP b "warning" = mkBlockType $ WarningB <$> warningP b
blockTypeP b "sidenote" = mkBlockType $ SideNoteB <$> sideNoteP b
blockTypeP _ "code" = mkBlockType codeP
blockTypeP b "connection" = mkBlockType $ connectionP b
blockTypeP b "example" = mkBlockType $ exampleP b
blockTypeP b "exercise" = mkBlockType $ exerciseP b
blockTypeP b "furtherreading" = mkBlockType $ furtherReadingP b
blockTypeP b "introduction" = mkBlockType $ introductionP b
blockTypeP b "summary" = mkBlockType $ summaryP b
blockTypeP b "intuition" = mkBlockType $ intuitionP b
blockTypeP _ "math" = mkBlockType $ MathB <$> mathP
blockTypeP _ "equation" = mkBlockType $ EquationB <$> equationP
blockTypeP _ "align" = mkBlockType $ AlignB <$> alignP
blockTypeP b "theorem" = mkBlockType $ TheoremB <$> theoremP b
blockTypeP b "lemma" = mkBlockType $ LemmaB <$> lemmaP b
blockTypeP b "corollary" = mkBlockType $ CorollaryB <$> corollaryP b
blockTypeP b "proposition" = mkBlockType $ PropositionB <$> propositionP b
blockTypeP b "conjecture" = mkBlockType $ conjectureP b
blockTypeP b "definition" = mkBlockType $ definitionP b
blockTypeP b "proof" = mkBlockType $ proofP b
blockTypeP b "axiom" = mkBlockType $ axiomP b
blockTypeP _ "image" = mkBlockType $ ImageB <$> imageP
blockTypeP _ "video" = mkBlockType $ VideoB <$> videoP
blockTypeP _ "youtube" = mkBlockType $ youTubeP
blockTypeP _ "prerex" = mkBlockType $ prerexP
blockTypeP _ "quote" = mkBlockType $ quoteP
blockTypeP b "recall" = mkBlockType $ recallP b
blockTypeP _ s = fail $ unpack $ "Did not recognize block type: " ++ s

makeLenses ''BlockType
