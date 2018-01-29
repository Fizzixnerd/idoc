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

import Data.Void

import qualified Text.Megaparsec as MP

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
compileIls t = compileIdoc markupTypeP blockTypeP "http://www.independentlearning.science/tiki/" t

compileIls' :: Text -> Either (MP.ParseError (MP.Token Text) (MP.ErrorFancy Void)) 
                              (Either (MP.ParseError (MP.Token IDocTokenStream) (MP.ErrorFancy Void))
                                      IlsDoc)
compileIls' t = compileIdoc' markupTypeP blockTypeP "http://www.independentlearning.science/tiki/" t

compileIdocTexFile :: (Markupy m, Blocky m (b m), MonadIO n) => Doc m b -> FilePath -> n ()
compileIdocTexFile doc_ outFile = liftIO $ renderFile outFile $ defaultDecorator (concatMap texy $ doc_^.docTitle.unDocTitle) (texy doc_ :: LaTeX)

compileIdocBook :: (Markupy m, Blocky m (b m), MonadIO n) => 
                   LaTeX -> Vector (Doc m b) -> FilePath -> n ()
compileIdocBook dTitle docs outFile = liftIO $ renderFile outFile $ defaultDecorator dTitle (concatMap texy docs)

blockTypeP :: BlockTypeName -> IDocParser m BlockType (BlockType m)
blockTypeP "info" = mkBlockType $ InfoB <$> infoP
blockTypeP "tip" = mkBlockType $ TipB <$> tipP
blockTypeP "caution" = mkBlockType $ CautionB <$> cautionP
blockTypeP "warning" = mkBlockType $ WarningB <$> warningP
blockTypeP "sidenote" = mkBlockType $ SideNoteB <$> sideNoteP
blockTypeP "bibliography" = mkBlockType $ bibTexP
blockTypeP "code" = mkBlockType codeP
blockTypeP "connection" = mkBlockType $ connectionP
blockTypeP "example" = mkBlockType $ exampleP
blockTypeP "exercise" = mkBlockType $ exerciseP
blockTypeP "furtherreading" = mkBlockType $ furtherReadingP
blockTypeP "introduction" = mkBlockType $ introductionP
blockTypeP "summary" = mkBlockType $ summaryP
blockTypeP "intuition" = mkBlockType $ intuitionP
blockTypeP "math" = mkBlockType $ MathB <$> mathP
blockTypeP "equation" = mkBlockType $ EquationB <$> equationP
blockTypeP "align" = mkBlockType $ AlignB <$> alignP
blockTypeP "theorem" = mkBlockType $ TheoremB <$> theoremP
blockTypeP "lemma" = mkBlockType $ LemmaB <$> lemmaP
blockTypeP "corollary" = mkBlockType $ CorollaryB <$> corollaryP
blockTypeP "proposition" = mkBlockType $ PropositionB <$> propositionP
blockTypeP "conjecture" = mkBlockType $ conjectureP
blockTypeP "definition" = mkBlockType $ definitionP
blockTypeP "proof" = mkBlockType $ proofP
blockTypeP "axiom" = mkBlockType $ axiomP
blockTypeP "image" = mkBlockType $ ImageB <$> imageP
blockTypeP "video" = mkBlockType $ VideoB <$> videoP
blockTypeP "youtube" = mkBlockType $ youTubeP
blockTypeP "prerex" = mkBlockType $ prerexP
blockTypeP "quote" = mkBlockType $ quoteP
blockTypeP "recall" = mkBlockType $ recallP
blockTypeP s = fail $ unpack $ "Did not recognize block type: " ++ s

markupTypeP :: MarkupTypeName -> IDocParser MarkupType b MarkupType
markupTypeP "footnote" = mkMarkupType $ footnoteP
markupTypeP s = fail $ unpack $ "Did not recognize markup type: " ++ s

makeLenses ''BlockType
makeLenses ''MarkupType
