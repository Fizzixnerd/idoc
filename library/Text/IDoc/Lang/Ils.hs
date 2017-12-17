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

instance BlockMarkup BlockType where
  blockMarkup attrs title_ sid (BlockType coRec) = blockMarkup attrs title_ sid coRec

x = blockMarkup (AttrMap $ mempty) Nothing Nothing (BlockType $ CoRec $ Identity $ Quote $ fromList [TextC $ "Hello!"])

mkBlockType :: (Functor f, RElem a IlsBlocks (RIndex a IlsBlocks)) => f a -> f BlockType
mkBlockType = (BlockType <$> CoRec <$> Identity <$>)

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

-- instance ToMarkup (Block BlockType) where
--   toMarkup Block { _bType = PrerexB p } = B.toMarkup p
--   toMarkup Block { _bType = MathB m } = B.toMarkup m
--   toMarkup Block { _bType = EquationB e } = B.toMarkup e
--   toMarkup Block { _bType = AlignB a } = B.toMarkup a
--   toMarkup b = (card blockCardOptions
--                      blockTitle
--                      (b^.bSetID)
--                      blockIcon
--                      blockFooter
--                      blockContents)
--     where
--       (blockCardOptions, blockTitle, blockIcon, blockFooter, blockContents) =
--         case b^.bType of
--           IntroductionB i -> (defaultCardOptions, mTitle "Introduction",
--                               "", Nothing, B.toMarkup i)
--           TheoremB (Theorem (bdy, foot)) -> (primaryCardOptions, mTitle "Theorem", theoremIcon,
--                                              (vectorBlockToMarkup "idocTheoremProof" id) <$> foot,
--                                              vectorBlockToMarkup "idocTheorem" id bdy)
--           LemmaB (Lemma (bdy, foot)) -> (primaryCardOptions, mTitle "Lemma", lemmaIcon,
--                                          (vectorBlockToMarkup "idocLemmaProof" id) <$> foot,
--                                          vectorBlockToMarkup "idocLemma" id bdy)
--           CorollaryB (Corollary (bdy, foot)) -> (primaryCardOptions, mTitle "Corollary",
--                                                  corollaryIcon,
--                                                  (vectorBlockToMarkup "idocCorollaryProof" id) <$> foot,
--                                                  vectorBlockToMarkup "idocCorollary" id bdy)
--           PropositionB (Proposition (bdy, foot)) -> (primaryCardOptions, mTitle "Proposition",
--                                                      propositionIcon,
--                                          (vectorBlockToMarkup "idocPropostionProof" id) <$> foot,
--                                          vectorBlockToMarkup "idocProposition" id bdy)
--           ConjectureB c -> (primaryCardOptions, mTitle "Conjecture",
--                             conjectureIcon, Nothing, B.toMarkup c)
--           AxiomB a -> (primaryCardOptions, mTitle "Axiom", axiomIcon,
--                        Nothing, B.toMarkup a)
--           ProofB p -> (primaryCardOptions, mTitle "Proof", proofIcon,
--                        Nothing, B.toMarkup p)
--           QuoteB q -> (defaultCardOptions { cardType = CInfo }, mTitle "Quote", quoteIcon,
--                        Nothing, B.toMarkup q)
--           CodeB c -> (defaultCardOptions, mTitle "Code", codeIcon,
--                       Nothing, B.toMarkup c)
--           ImageB (Image (il, foot)) -> (defaultCardOptions, mTitle "Image", imageIcon,
--                        (vectorBlockToMarkup "idocImageFooter" id) <$> foot,
--                        B.img B.! A.class_ "idocImage img-responsive"
--                              B.! A.src (B.toValue $ il^.linkLocation))
--           VideoB (Video (vl, foot)) -> (defaultCardOptions, mTitle "Video", videoIcon,
--                        (vectorBlockToMarkup "idocVideoFooter" id) <$> foot, 
--                        B.video B.! A.class_ "idocVideo"
--                                B.! A.controls "true"
--                                B.! A.src (B.toValue $ vl^.linkLocation) $
--                                "")
--           ConnectionB c -> (primaryCardOptions, mTitle "Connection",
--                             connectionIcon, Nothing, B.toMarkup c)
--           DefinitionB d -> (primaryCardOptions, mTitle "Defintion",
--                             definitionIcon, Nothing, B.toMarkup d)
--           IntuitionB i -> (defaultCardOptions { cardType = CInfo }, mTitle "Intuition",
--                            intuitionIcon, Nothing, B.toMarkup i)
--           YouTubeB (YouTube (yl, foot)) -> (defaultCardOptions, mTitle "Youtube",
--                                             youTubeIcon,
--                                             (vectorBlockToMarkup "idocYouTubeFooter" id) <$> foot,
--                                             B.div B.! A.class_ "embed-responsive embed-responsive-16by9" $
--                                                   B.iframe B.! A.class_ "idocYouTubeEmbed embed-responsive-item"
--                                                            B.! allowFullscreen "true"
--                                                            B.! A.src (B.toValue yl) $
--                                                            "")
--             where allowFullscreen = B.customAttribute "allowfullscreen"
--           InfoB a -> (defaultCardOptions { cardType = CInfo
--                                          },
--                       mTitle "Info", infoIcon, Nothing, B.toMarkup a)
--           TipB a -> (defaultCardOptions { cardType = CPrimary
--                                         },
--                      mTitle "Tip", tipIcon, Nothing, B.toMarkup a)
--           CautionB a -> (defaultCardOptions { cardType = CWarning
--                                             },
--                          mTitle "Caution", cautionIcon, Nothing, B.toMarkup a)
--           WarningB a -> (defaultCardOptions { cardType = CDanger
--                                             },
--                          mTitle "Warning", warningIcon, Nothing, B.toMarkup a)
--           SideNoteB s -> ( primaryCardOptions
--                          , mTitle "Side Note"
--                          , sideNoteIcon
--                          , Nothing
--                          , B.toMarkup s)
--           ExampleB (Example (ex, ans)) -> (primaryCardOptions, mTitle "Example", exampleIcon,
--                                            Just $ vectorBlockToMarkup "idocExampleAnswer" id ans, 
--                                            vectorBlockToMarkup "idocExample" id ex)
--           ExerciseB e -> (primaryCardOptions, mTitle "Exercise", exerciseIcon,
--                           Nothing, B.toMarkup e)
--           BibliographyB b_ -> (defaultCardOptions, mTitle "Bibliography",
--                                bibliographyIcon, Nothing, B.toMarkup b_)
--           FurtherReadingB f -> (defaultCardOptions, mTitle "Further Reading",
--                                 furtherReadingIcon, Nothing, B.toMarkup f)
--           SummaryB s -> (defaultCardOptions, mTitle "Summary", summaryIcon,
--                          Nothing, B.toMarkup s)
--           RecallB (Recall (vl, foot)) -> (defaultCardOptions { cardType = CInfo }, mTitle "Recall", recallIcon,
--                                           Just $ vectorBlockToMarkup "idocRecallFooter" id foot,
--                                           vectorBlockToMarkup "idocRecall" id vl)
--           x -> error $ "Unhandled case: " ++ show x
--       mTitle defaultTitle = maybe defaultTitle B.toMarkup (b^.bTitle)
--       theoremIcon = icon "fa-star-o"
--       lemmaIcon = theoremIcon
--       corollaryIcon = theoremIcon
--       propositionIcon = theoremIcon
--       conjectureIcon = icon "fa-question"
--       axiomIcon = icon "fa-cube"
--       proofIcon = icon "fa-star"
--       quoteIcon = icon "fa-quote-left"
--       codeIcon = icon "fa-code"
--       imageIcon = icon "fa-image"
--       videoIcon = "" -- FIXME: Find an icon for this
--       connectionIcon = icon "fa-link"
--       definitionIcon = axiomIcon
--       intuitionIcon = icon "fa-puzzle-piece"
--       sideNoteIcon = icon "fa-sticky-note-o"
--       exampleIcon = icon "fa-pencil"
--       exerciseIcon = icon "fa-question-circle"
--       bibliographyIcon = "" -- FIXME
--       furtherReadingIcon = "" -- FIXME
--       summaryIcon = "" -- FIXME
--       recallIcon = "" -- FIXME
