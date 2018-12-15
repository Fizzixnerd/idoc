{-# LANGUAGE DataKinds #-}

module Text.IDoc.Lang.Ils where

import ClassyPrelude hiding (Identity)

import qualified Data.List.NonEmpty as NE

import Control.Lens hiding (Identity, List)

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
import Text.IDoc.Blocks.Lists
import Text.IDoc.Blocks.Math
import Text.IDoc.Blocks.Media
import Text.IDoc.Blocks.Prerex
import Text.IDoc.Blocks.Quote
import Text.IDoc.Blocks.Recall
import Text.IDoc.Markup.Footnote
import Text.IDoc.Markup.FootnoteRef
import Text.IDoc.Markup.Citation
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
                    , ListB m BlockType
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

instance ( CheckLinks m BlockType m
         , CheckLinks m BlockType (BlockType m)) =>
  CheckLinks m BlockType (Connection m BlockType) where
  checkLinks constraints@(Constraints { _lcBackConstraints = backConstraints }) container (Connection c) =
    let mPrerex = do
          (CC (BlockC (Block { _bType = (BlockType bty) }))) <- headMay c
          asA bty :: Maybe (Prerex m)
        extraConstraints = maybe LinkNone toConstraints mPrerex
        connectionConstraints = constraints
                                { _lcBackConstraints =
                                    backConstraints <> extraConstraints
                                }
    in
      concatMap (checkLinks connectionConstraints container) c


newtype BlockType m = BlockType { _unBlockType :: CoRec Identity (IlsBlocks m) }
  deriving (Eq, Show)

instance BlockMarkup MarkupType (BlockType MarkupType) where
  blockMarkup attrs title_ sid (BlockType coRec) = blockMarkup attrs title_ sid coRec

instance Blocky MarkupType (BlockType MarkupType) where
  blocky attrs title_ sid (BlockType coRec) = blocky attrs title_ sid coRec

instance CheckLinks MarkupType BlockType (BlockType MarkupType) where
  checkLinks constraints container (BlockType coRec) = checkLinks constraints container coRec

type IlsMarkup = '[ Footnote MarkupType
                  , FootnoteRef
                  , Citation ]

newtype MarkupType = MarkupType { _unMarkupType :: CoRec Identity IlsMarkup }
  deriving (Eq, Show)

instance MarkupMarkup MarkupType where
  markupMarkup attrs sid (MarkupType coRec) = markupMarkup attrs sid coRec

instance Markupy MarkupType where
  markupy attrs sid (MarkupType coRec) = markupy attrs sid coRec

instance CheckLinks MarkupType BlockType MarkupType where
  checkLinks constraints container (MarkupType coRec) = checkLinks constraints container coRec

type IlsDoc = Doc MarkupType BlockType

mkBlockType :: (Functor f, RElem a (IlsBlocks m) (RIndex a (IlsBlocks m))) => f a -> f (BlockType m)
mkBlockType = (BlockType <$> CoRec <$> Identity <$>)

mkMarkupType :: (Functor f, RElem a IlsMarkup (RIndex a IlsMarkup)) => f a -> f MarkupType
mkMarkupType = (MarkupType <$> CoRec <$> Identity <$>)

compileIls :: Bool -> Text -> IlsDoc
compileIls development t = compileIdoc markupTypeP blockTypeP
               (if development
                then "https://localhost:3443/tiki/"
                else "https://independentlearning.science/tiki/")
               (if development
                 then "https://localhost:3443/tiki/media/image/"
                 else "https://independentlearning.science/tiki/media/image/")
               (if development
                 then "https://localhost:3443/tiki/media/audio/"
                 else "https://independentlearning.science/tiki/media/audio/")
               t

compileIls' :: Bool -> Text -> Either (MP.ParseErrorBundle Text Void)
                              (Either (MP.ParseErrorBundle IDocTokenStream Void)
                                      IlsDoc)
compileIls' development t = compileIdoc' markupTypeP blockTypeP
                            (if development
                             then "https://localhost:3443/tiki/"
                             else "https://independentlearning.science/tiki/")
                            (if development
                             then "https://localhost:3443/tiki/media/image/"
                             else "https://independentlearning.science/tiki/media/image/")
                            (if development
                             then "https://localhost:3443/tiki/media/audio/"
                             else "https://independentlearning.science/tiki/media/audio/")
                            t

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
blockTypeP "bibliography" = mkBlockType bibTexP
blockTypeP "code" = mkBlockType codeP
blockTypeP "connection" = mkBlockType connectionP
blockTypeP "example" = mkBlockType exampleP
blockTypeP "exercise" = mkBlockType exerciseP
blockTypeP "furtherreading" = mkBlockType furtherReadingP
blockTypeP "introduction" = mkBlockType introductionP
blockTypeP "summary" = mkBlockType summaryP
blockTypeP "intuition" = mkBlockType intuitionP
blockTypeP "math" = mkBlockType $ MathB <$> mathP
blockTypeP "equation" = mkBlockType $ EquationB <$> equationP
blockTypeP "align" = mkBlockType $ AlignB <$> alignP
blockTypeP "theorem" = mkBlockType $ TheoremB <$> theoremP
blockTypeP "lemma" = mkBlockType $ LemmaB <$> lemmaP
blockTypeP "corollary" = mkBlockType $ CorollaryB <$> corollaryP
blockTypeP "proposition" = mkBlockType $ PropositionB <$> propositionP
blockTypeP "conjecture" = mkBlockType conjectureP
blockTypeP "definition" = mkBlockType definitionP
blockTypeP "proof" = mkBlockType proofP
blockTypeP "axiom" = mkBlockType axiomP
blockTypeP "image" = mkBlockType $ ImageB <$> imageP
blockTypeP "video" = mkBlockType $ VideoB <$> videoP
blockTypeP "audio" = mkBlockType $ AudioB <$> audioP
blockTypeP "youtube" = mkBlockType youTubeP
blockTypeP "prerex" = mkBlockType prerexP
blockTypeP "quote" = mkBlockType quoteP
blockTypeP "recall" = mkBlockType recallP
blockTypeP "orderedlist" = mkBlockType $ OListB <$> orderedListP
blockTypeP "unorderedlist" = mkBlockType $ UListB <$> unorderedListP
blockTypeP "descriptionlist" = mkBlockType $ DListB <$> descriptionListP
blockTypeP s = MP.unexpected (MP.Label $ NE.fromList $ "Block type")

markupTypeP :: MarkupTypeName -> IDocParser MarkupType b MarkupType
markupTypeP "footnote" = mkMarkupType $ footnoteP
markupTypeP "footnotref" = mkMarkupType $ footnoteRefP
markupTypeP "cite" = mkMarkupType $ citationP
markupTypeP s = fail $ unpack $ "Did not recognize markup type: " ++ s

makeLenses ''BlockType
makeLenses ''MarkupType

