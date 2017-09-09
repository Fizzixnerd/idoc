{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards #-}
-- | Html5.hs
-- Author: Matt Walker
-- License: https://opensource.org/licenses/BSD-2-Clause
-- Created: Aug 30, 2017
-- Summary: 

module Text.IDoc.Render.Html5 where

import Text.IDoc.Syntax
import Text.IDoc.Lex
import Text.IDoc.Parse
import Text.IDoc.Render.Html5.Icons

import System.IO

import Control.Lens hiding (cons, List)

import qualified Text.Blaze.Html5 as B
import qualified Text.Blaze.Html5.Attributes as A
import qualified Text.Blaze.Html.Renderer.String as S

import qualified Data.Vector as V

import ClassyPrelude as CP
import qualified Text.Megaparsec as MP
import Text.Megaparsec.Text

mID :: Maybe SetID -> (B.Html -> B.Html)
mID mid = case mid of
  Nothing -> id
  Just id_ -> (\x -> x B.! A.id (B.toValue id_))

data DefaultCollapseState = Collapsed 
                          | Uncollapsed deriving (Eq, Show)

instance B.ToValue DefaultCollapseState where
  toValue Collapsed = ""
  toValue Uncollapsed = "in"

data PanelType = PDefault 
               | PPrimary 
               | PInfo 
               | PSuccess
               | PWarning
               | PDanger deriving (Eq, Show)

instance B.ToValue PanelType where
  toValue PDefault = "panel-default"
  toValue PPrimary = "panel-primary"
  toValue PInfo    = "panel-info"
  toValue PSuccess = "panel-success"
  toValue PWarning = "panel-warning"
  toValue PDanger  = "panel-danger"

data GridWidth = GridFour
               | GridSix
               | GridEight
               | GridTwelve deriving (Eq, Show)

instance B.ToValue GridWidth where
  toValue GridFour = "col-md-4"
  toValue GridSix = "col-md-6"
  toValue GridEight = "col-md-8"
  toValue GridTwelve = "col-md-12"

data PanelOptions = PanelOptions { panelDefaultCollapseState :: DefaultCollapseState
                                 , panelType :: PanelType
                                 , panelGridWidth :: GridWidth
                                 } deriving (Eq, Show)

defaultPanelOptions :: PanelOptions
defaultPanelOptions = PanelOptions Uncollapsed PDefault GridTwelve

panel :: B.ToValue a => 
         PanelOptions 
      -> B.Html -- ^ title
      -> Maybe a -- ^ id
      -> Icon -- ^ icon
      -> Maybe B.Html -- ^ footer
      -> B.Html -- ^ body
      -> B.Html
panel (PanelOptions {..}) title_ id_ icon__ footer_ body_= 
  B.div B.! A.class_ (B.toValue panelGridWidth) $
  B.div B.! A.class_ "card" $
               (B.h5 B.! A.class_ "card-header card-inverse idocCardHeader bg-primary" $
                    (mHrefV id_ $ 
                     B.a B.! A.class_ "idocCardHeaderLink"
                         B.! B.dataAttribute "toggle" "collapse" $
                     (icon__ ++ " " ++ title_ ++ " " ++ (B.span B.! A.class_ "fa fa-angle-double-down" $ "")))) ++
         (mID' id_ $ 
          B.div B.! A.class_ "card-collapse show" $
                mfooterify footer_ $ (B.div B.! A.class_ "card-block" $
                                            body_))
  where
    mfooterify Nothing = id
    mfooterify (Just f) = ((flip (++)) (B.div B.! A.class_ "card-footer" $ f))
    mHrefV (Just i') = (B.! A.href ("#" ++ (B.toValue i')))
    mHrefV Nothing  = id
    mID' (Just i') = (B.! A.id (B.toValue i'))
    mID' Nothing = id

instance B.ToMarkup Core where
  toMarkup (SC sc) = B.toMarkup sc
  toMarkup (CC cc) = B.toMarkup cc

instance B.ToMarkup SimpleCore where
  toMarkup (TextC t) = B.toMarkup t
  toMarkup (QTextC qt) = B.toMarkup qt
  toMarkup (LinkC l) = B.toMarkup l
  toMarkup (InlineMathC im) = B.toMarkup im
  toMarkup (MarkupC m) = B.toMarkup m

instance B.ToMarkup ComplexCore where
  toMarkup (ListC l) = B.toMarkup l
  toMarkup (BlockC b) = B.toMarkup b
  toMarkup (ParagraphC p) = B.toMarkup p
  
instance B.ToMarkup QText where
  toMarkup qt = decorateTextWith (qt^.qtType) $
                concatMap B.toMarkup $ qt^.qtText
    where
      decorateTextWith Strong x = B.strong B.! A.class_ "idocStrong" $ x
      decorateTextWith Emphasis x = B.em B.! A.class_ "idocEmphasis" $ x
      decorateTextWith Monospace x = B.span B.! A.class_ "idocMonospace" $ x
      decorateTextWith Superscript x = B.sup B.! A.class_ "idocSuperscript" $ x
      decorateTextWith Subscript x = B.sub B.! A.class_ "idocSubscript" $ x
      decorateTextWith Quoted x = B.q B.! A.class_ "idocQuoted" $ x

instance B.ToValue LinkType where
  toValue Internal = "idocInternal"
  toValue Back = "idocBackLink"
  toValue Out = "idocOutLink"

idHelper :: (Text -> t) -> ID -> t
idHelper decorator id_ = let (base, hash_) =
                               case (id_^.idProtocol, id_^.idHash) of
                                 (Just (Protocol "youtube"), Nothing) -> 
                                   ("https://youtube.com/embed/", "")
                                 (Just (Protocol "youtube"), Just _) -> 
                                   error "got youtube protocol with a hash!?"
                                 (Nothing, Nothing) -> 
                                   ("http://www.independentlearning.science/", "")
                                 (Just (Protocol p), Just (IDHash h)) ->
                                   (p ++ "://", h)
                                 (Nothing, Just (IDHash h)) -> ("/", h)
                                 (Just (Protocol p), Nothing) -> (p ++ "://", "")
    in
      decorator $ base ++
      (concatMap (\(IDBase x) -> x) $ intersperse (IDBase "/") (id_^.idBase)) ++
      hash_

instance B.ToMarkup ID where
  toMarkup id_ = idHelper B.toMarkup id_

instance B.ToValue ID where
  toValue id_ = idHelper B.toValue id_

instance B.ToValue SetID where
  toValue (SetID (IDHash sid)) = B.toValue sid

instance B.ToValue Link where
  toValue l = B.toValue $ l^.linkLocation

instance B.ToMarkup Link where
  toMarkup l = B.a B.! A.class_ (B.toValue $ l^.linkType)
                   B.! A.href (B.toValue $ l^.linkLocation) $
                   B.toMarkup $ l^.linkText

instance B.ToMarkup LinkText where
  toMarkup (LinkText lt) = concatMap B.toMarkup lt

instance B.ToMarkup InlineMath where
  toMarkup im = (mID (im^.imSetID) (B.span $ ("\\(" ++ concatMap B.toMarkup (im^.imContents) ++ "\\)"))) B.! A.class_ "idocInlineMath" 

instance B.ToMarkup Token where
  toMarkup t = B.toMarkup $ unToken t

instance B.ToMarkup Markup where
  toMarkup mu = mID (mu^.muSetID) $
    case (mu^.muType) of
      Footnote -> B.span B.! A.class_ "idocFootnote" $ 
                  concatMap B.toMarkup (mu^.muContents)
      FootnoteRef -> B.span B.! A.class_ "idocFootnoteRef" $
                     concatMap B.toMarkup (mu^.muContents)
      Citation -> B.span B.! A.class_ "idocCitation" $
                  concatMap B.toMarkup (mu^.muContents)

instance B.ToMarkup Paragraph where
  toMarkup p = B.p B.! A.class_ "idocParagraph" $
               concatMap B.toMarkup $ p^.paraContents

vectorBlockToMarkup :: B.ToMarkup a => 
                       B.AttributeValue 
                    -> (B.Html -> B.Html)
                    -> Vector a 
                    -> B.Html
vectorBlockToMarkup cls dec vb = B.div B.! A.class_ cls $
                                 dec $
                                 concatMap B.toMarkup
                                 vb

verbatimBlockToMarkup :: B.AttributeValue 
                      -> (B.Html -> B.Html)
                      -> Vector Token
                      -> B.Html
verbatimBlockToMarkup cls dec vb = B.div B.! A.class_ cls $
                                   dec $
                                   concatMap (\x -> if x == Newline then
                                                      B.toMarkup B.br
                                                    else
                                                      B.toMarkup x)
                                   vb

instance B.ToMarkup BlockTitle where
  toMarkup (BlockTitle bt) = concatMap B.toMarkup bt

instance B.ToMarkup Block where
  toMarkup Block { _bType = PrerexB p } = B.toMarkup p
  toMarkup b = (B.div B.! A.class_ "clearfix" $ "") ++
               (panel blockPanelOptions 
                      blockTitle 
                      (b^.bSetID)
                      blockIcon
                      blockFooter
                      blockContents)
    where
      (blockPanelOptions, blockTitle, blockIcon, blockFooter, blockContents) =
        case b^.bType of
          IntroductionB i -> (defaultPanelOptions, mTitle "Introduction",
                              "", Nothing, B.toMarkup i)
          MathB m -> (defaultPanelOptions, mTitle "Math", "",
                      Nothing, B.toMarkup m)
          EquationB e -> (defaultPanelOptions, mTitle "Equation", "",
                          Nothing, B.toMarkup e)
          EqnArrayB e -> (defaultPanelOptions, mTitle "EqnArray", "",
                          Nothing, B.toMarkup e)
          TheoremB (Theorem (bdy, foot)) -> (defaultPanelOptions, mTitle "Theorem", theoremIcon,
                                             (vectorBlockToMarkup "idocTheoremProof" id) <$> foot,
                                             vectorBlockToMarkup "idocTheorem" id bdy)
          LemmaB (Lemma (bdy, foot)) -> (defaultPanelOptions, mTitle "Lemma", lemmaIcon,
                                         (vectorBlockToMarkup "idocLemmaProof" id) <$> foot,
                                         vectorBlockToMarkup "idocLemma" id bdy)
          CorollaryB (Corollary (bdy, foot)) -> (defaultPanelOptions, mTitle "Corollary",
                                                 corollaryIcon,
                                                 (vectorBlockToMarkup "idocCorollaryProof" id) <$> foot,
                                                 vectorBlockToMarkup "idocCorollary" id bdy)
          PropositionB (Proposition (bdy, foot)) -> (defaultPanelOptions, mTitle "Proposition",
                                                     propositionIcon,
                                         (vectorBlockToMarkup "idocPropostionProof" id) <$> foot,
                                         vectorBlockToMarkup "idocProposition" id bdy)
          ConjectureB c -> (defaultPanelOptions, mTitle "Conjecture",
                            conjectureIcon, Nothing, B.toMarkup c)
          AxiomB a -> (defaultPanelOptions, mTitle "Axiom", axiomIcon,
                       Nothing, B.toMarkup a)
          ProofB p -> (defaultPanelOptions, mTitle "Proof", proofIcon,
                       Nothing, B.toMarkup p)
          QuoteB q -> (defaultPanelOptions, mTitle "Quote", quoteIcon,
                       Nothing, B.toMarkup q)
          CodeB c -> (defaultPanelOptions, mTitle "Code", codeIcon,
                      Nothing, B.toMarkup c)
          ImageB (Image (il, foot)) -> (defaultPanelOptions, mTitle "Image", imageIcon,
                       (vectorBlockToMarkup "idocImageFooter" id) <$> foot,
                       B.img B.! A.class_ "idocImage img-responsive"
                             B.! A.src (B.toValue $ il^.linkLocation))
          VideoB (Video (vl, foot)) -> (defaultPanelOptions, mTitle "Video", videoIcon,
                       (vectorBlockToMarkup "idocVideoFooter" id) <$> foot, 
                       B.video B.! A.class_ "idocVideo"
                               B.! A.controls "true"
                               B.! A.src (B.toValue $ vl^.linkLocation) $
                               "")
          ConnectionB c -> (defaultPanelOptions, mTitle "Connection",
                            connectionIcon, Nothing, B.toMarkup c)
          DefinitionB d -> (defaultPanelOptions, mTitle "Defintion",
                            definitionIcon, Nothing, B.toMarkup d)
          IntuitionB i -> (defaultPanelOptions, mTitle "Intuition",
                           intuitionIcon, Nothing, B.toMarkup i)
          YouTubeB (YouTube (yl, foot)) -> (defaultPanelOptions, mTitle "Youtube",
                                            youTubeIcon,
                                            (vectorBlockToMarkup "idocYouTubeFooter" id) <$> foot,
                                            B.div B.! A.class_ "embed-responsive embed-responsive-16by9" $
                                                  B.iframe B.! A.class_ "idocYouTubeEmbed embed-responsive-item"
                                                           B.! allowFullscreen "true"
                                                           B.! A.src (B.toValue yl) $
                                                           "")
            where allowFullscreen = B.customAttribute "allowfullscreen"
          InfoB a -> (defaultPanelOptions { panelType = PInfo
                                          , panelGridWidth = GridFour
                                          },
                      mTitle "Info", infoIcon, Nothing, B.toMarkup a)
          TipB a -> (defaultPanelOptions { panelType = PPrimary
                                         , panelGridWidth = GridFour
                                         },
                     mTitle "Tip", tipIcon, Nothing, B.toMarkup a)
          CautionB a -> (defaultPanelOptions { panelType = PWarning
                                             , panelGridWidth = GridFour
                                             },
                         mTitle "Caution", cautionIcon, Nothing, B.toMarkup a)
          WarningB a -> (defaultPanelOptions { panelType = PDanger
                                             , panelGridWidth = GridFour
                                             },
                         mTitle "Warning", warningIcon, Nothing, B.toMarkup a)
          SideNoteB s -> (defaultPanelOptions { panelGridWidth = GridFour },
                          mTitle "Side Note", sideNoteIcon, Nothing,
                          B.toMarkup s)
          ExampleB (Example (ex, ans)) -> (defaultPanelOptions, mTitle "Example", exampleIcon,
                                           Just $ vectorBlockToMarkup "idocExampleAnswer" id ans, 
                                           vectorBlockToMarkup "idocExample" id ex)
          ExerciseB e -> (defaultPanelOptions, mTitle "Exercise", exerciseIcon,
                          Nothing, B.toMarkup e)
          BibliographyB b_ -> (defaultPanelOptions, mTitle "Bibliography",
                               bibliographyIcon, Nothing, B.toMarkup b_)
          FurtherReadingB f -> (defaultPanelOptions, mTitle "Further Reading",
                                furtherReadingIcon, Nothing, B.toMarkup f)
          SummaryB s -> (defaultPanelOptions, mTitle "Summary", summaryIcon,
                         Nothing, B.toMarkup s)
          RecallB (Recall (vl, foot)) -> (defaultPanelOptions, mTitle "Recall", recallIcon,
                                          Just $ vectorBlockToMarkup "idocRecallFooter" id foot,
                                          vectorBlockToMarkup "idocRecall" id vl)
          x -> error $ "Unhandled case: " ++ show x
      mTitle defaultTitle = maybe defaultTitle B.toMarkup (b^.bTitle)
      theoremIcon = icon "fa-star-o"
      lemmaIcon = theoremIcon
      corollaryIcon = theoremIcon
      propositionIcon = theoremIcon
      conjectureIcon = icon "fa-question"
      axiomIcon = icon "fa-cube"
      proofIcon = icon "fa-star"
      quoteIcon = icon "fa-quote-left"
      codeIcon = icon "fa-code"
      imageIcon = icon "fa-image"
      videoIcon = "" -- FIXME: Find an icon for this
      connectionIcon = icon "fa-link"
      definitionIcon = axiomIcon
      intuitionIcon = icon "fa-puzzle-piece"
      sideNoteIcon = icon "fa-sticky-note-o"
      exampleIcon = icon "fa-pencil"
      exerciseIcon = icon "fa-question-circle"
      bibliographyIcon = "" -- FIXME
      furtherReadingIcon = "" -- FIXME
      summaryIcon = "" -- FIXME
      recallIcon = "" -- FIXME

instance B.ToMarkup PrerexItem where
  toMarkup p = panel (defaultPanelOptions { panelType = PInfo
                                          , panelDefaultCollapseState = Collapsed
                                          })
                     (B.toMarkup $ p^.prerexItemPath)
                     (Just $ p^.prerexItemPath)
                     prerexItemIcon
                     (Just $ B.a B.! A.class_ "idocPrerexItemLink"
                                 B.! A.href (p^.prerexItemPath.to B.toValue) $
                                 "Go to " ++ (p^.prerexItemPath.to B.toMarkup))
                     (concatMap B.toMarkup $ p^.prerexItemDescription)

instance B.ToMarkup Prerex where
  toMarkup p = B.div B.! A.class_ "idocPrerex panel-group" $ 
               concatMap B.toMarkup (p^.prerexContents)

instance B.ToMarkup Introduction where
  toMarkup (Introduction i) = vectorBlockToMarkup "idocIntroduction" id i

instance B.ToMarkup Math where
  toMarkup (Math m) = vectorBlockToMarkup "idocMath center-block flex" 
                      (\x -> "\\[" ++ x ++ "\\]") m

instance B.ToMarkup Equation where
  toMarkup (Equation e) = vectorBlockToMarkup "idocEquation center-block flex" 
                          (\x -> "$$\\begin{equation}\n" ++ x ++ 
                                 "\\end{equation}$$") e

instance B.ToMarkup EqnArray where
  toMarkup (EqnArray ea) = vectorBlockToMarkup "idocEqnArray center-block"
                           (\x -> "$$\\begin{eqnarray}\n" ++ 
                                  x ++
                                  "\\end{eqnarray}$$") $
                           (reverse (V.foldl (\acc y -> if y == Newline then  
                                                          fromList [BSlash, BSlash] <> acc
                                                        else
                                                          V.cons y acc) empty ea))

-- instance B.ToMarkup Theorem where
--   toMarkup (Theorem t) = vectorBlockToMarkup "idocTheorem" id t

-- instance B.ToMarkup Lemma where
--   toMarkup (Lemma l) = vectorBlockToMarkup "idocLemma" id l

-- instance B.ToMarkup Corollary where
--   toMarkup (Corollary c) = vectorBlockToMarkup "idocCorollary" id c

-- instance B.ToMarkup Proposition where
--   toMarkup (Proposition p) = vectorBlockToMarkup "idocProposition" id p

instance B.ToMarkup Conjecture where
  toMarkup (Conjecture c) = vectorBlockToMarkup "idocConjecture" id c

instance B.ToMarkup Axiom where
  toMarkup (Axiom a) = vectorBlockToMarkup "idocAxiom" id a

instance B.ToMarkup Proof where
  toMarkup (Proof p) = vectorBlockToMarkup "idocProof" id p

instance B.ToMarkup Quote where
  toMarkup (Quote q) = vectorBlockToMarkup "idocQuote" id q

instance B.ToMarkup Code where
  toMarkup (Code c) = verbatimBlockToMarkup "idocCode" id c

-- instance B.ToMarkup Image where
--   toMarkup (Image il) = B.img B.! A.class_ "idocImage img-responsive"
--                               B.! A.src (B.toValue $ il^.linkLocation)

-- instance B.ToMarkup Video where
--   toMarkup (Video vl) = B.video B.! A.class_ "idocVideo"
--                                 B.! A.controls "true"
--                                 B.! A.src (B.toValue $ vl^.linkLocation) $
--                                 ""

-- instance B.ToMarkup YouTube where
--   toMarkup (YouTube yl) = B.div B.! A.class_ "embed-responsive embed-responsive-16by9" $
--                                  B.iframe B.! A.class_ "idocYouTubeEmbed embed-responsive-item"
--                                           B.! allowFullscreen "true"
--                                           B.! A.src (B.toValue yl) $
--                                           ""
--     where
      

instance B.ToMarkup Connection where
  toMarkup (Connection c) = vectorBlockToMarkup "idocConnection" id c

instance B.ToMarkup Definition where
  toMarkup (Definition d) = vectorBlockToMarkup "idocDefinition" id d

instance B.ToMarkup Intuition where
  toMarkup (Intuition i) = vectorBlockToMarkup "idocIntuition" id i

decorateAdmonition :: PanelType -> B.Html -> B.Html
decorateAdmonition pt cnt = (B.span B.! A.class_ 
                             ("fa " ++
                              faIcon ++
                              " fa-4x fa-pull-left fa-border") $ "") ++ cnt
  where
    faIcon = case pt of
      PInfo -> "fa-info-circle"
      PDanger -> "fa-exclamation-circle"
      PWarning -> "fa-exclamation-triangle"
      PPrimary -> "fa-lightbulb-o"
      _ -> error "I can't decorate like that!"

instance B.ToMarkup Info where
  toMarkup (Info a) = decorateAdmonition PInfo $
                      vectorBlockToMarkup "idocInfo" id a

instance B.ToMarkup Tip where
  toMarkup (Tip a) = decorateAdmonition PPrimary $ 
                     vectorBlockToMarkup "idocTip" id a

instance B.ToMarkup Caution where
  toMarkup (Caution a) = decorateAdmonition PWarning $ 
                         vectorBlockToMarkup "idocCaution" id a

instance B.ToMarkup Warning where
  toMarkup (Warning a) = decorateAdmonition PDanger $ 
                         vectorBlockToMarkup "idocWarning" id a

instance B.ToMarkup SideNote where
  toMarkup (SideNote s) = vectorBlockToMarkup "idocSideNote" id s

-- instance B.ToMarkup Example where
--   toMarkup (Example e) = vectorBlockToMarkup "idocExample" id e

instance B.ToMarkup Exercise where
  toMarkup (Exercise e) = vectorBlockToMarkup "idocExercise" id e

instance B.ToMarkup Bibliography where
  toMarkup (Bibliography b) = vectorBlockToMarkup "idocBibliography" id b

instance B.ToMarkup BibItem where
  toMarkup bi = "author: " ++ (bi^.biAuthor.to B.toMarkup) ++
                " title: " ++ (bi^.biTitle.to B.toMarkup) ++
                " year: " ++ (bi^.biYear.to B.toMarkup)

instance B.ToMarkup FurtherReading where
  toMarkup (FurtherReading f) = vectorBlockToMarkup "idocFurtherReading" id f

instance B.ToMarkup Summary where
  toMarkup (Summary s) = vectorBlockToMarkup "idocSummary" id s

-- instance B.ToMarkup Recall where
--   toMarkup (Recall r) = vectorBlockToMarkup "idocRecall" id r

instance B.ToMarkup ListItem where
  toMarkup li = correctListItemHolder (li^.liType) (li^.liLabel) $ 
                concatMap B.toMarkup $ li^.liContents
    where
      correctListItemHolder Labelled (Just l) = 
        (\x -> (B.dt B.! A.class_ "idocLabel" $ B.toMarkup l) ++
               (B.dd B.! A.class_ "idocLabelledItem" $ x))
      correctListItemHolder Ordered Nothing = B.li B.! A.class_ "idocOrderedItem"
      correctListItemHolder Unordered Nothing = 
        B.li B.! A.class_ "idocUnorderedItem"
      correctListItemHolder lt l = error $ 
                                  "Failed to match pattern with ListType `" ++
                                  show lt ++
                                  "' and label `" ++
                                  show l ++
                                  "'."

instance B.ToMarkup ListLabel where
  toMarkup (ListLabel ll) = concatMap B.toMarkup ll

instance B.ToMarkup List where
  toMarkup (List l) = correctListHolder ((V.head l)^.liType) $
                      concatMap B.toMarkup l
    where
      correctListHolder Unordered = B.ul B.! A.class_ "idocUnorderedList"
      correctListHolder Ordered = B.ol B.! A.class_ "idocOrderedList"
      correctListHolder Labelled = B.dl B.! A.class_ "idocLabelledList"

instance B.ToMarkup SectionTitle where
  toMarkup (SectionTitle st) = concatMap B.toMarkup st

instance B.ToMarkup Section where
  toMarkup s = B.section B.! A.class_ "idocSection" $
               titlify $ concatMap B.toMarkup $ s^.secContents
    where titlify = case s^.secType of
            Preamble -> id
            TopSection -> (mID (s^.secSetID) 
                            (B.h2 B.! A.class_ "idocTopSectionTitle clearfix" $
                             s^.secTitle.to B.toMarkup) ++)
            SubSection -> (mID (s^.secSetID) 
                            (B.h3 B.! A.class_ "idocSubSectionTitle clearfix" $
                             s^.secTitle.to B.toMarkup) ++)

instance B.ToMarkup DocTitle where
  toMarkup (DocTitle dt) = B.h1 B.! A.class_ "idocDocTitle" $
                           concatMap B.toMarkup dt

instance B.ToMarkup Doc where
  toMarkup d = B.article B.! A.class_ "idocDoc" $
               (B.toMarkup $ d^.docTitle) ++
               (concatMap B.toMarkup $ d^.docSections)

megaMain'' :: IO ()
megaMain'' = (withFile "simple1.idoc" ReadMode 
             (\src -> do
                cnts <- CP.hGetContents src
                case MP.parse (Text.IDoc.Lex.dTokens :: Parser IDocTokenStream) "source.idoc" (CP.decodeUtf8 cnts) of
                  (CP.Right x) -> do
                    --System.IO.hPutStr tex (Data.Text.unpack $ utRender x)
                    case MP.parse docP "<tokens>" x of
                     CP.Right y -> CP.putStrLn $ fromString $ renderPretty y
                     CP.Left z -> CP.print z))

html x = B.docTypeHtml $ B.html $
  (B.head $ (B.meta B.! A.charset "utf-8") ++
            (B.meta B.! A.name "viewport"
                    B.! A.content "width=device-width, initial-scale=1, shrink-to-fit=no") ++
            (B.script $ B.text $ unlines $ [ "window.MathJax = { TeX: {equationNumbers: {autoNumber: \"AMS\", formatID: function (n) {return ''+String(n).replace(/[:'\"<>&]/g,\"\")}}}}"
                                           ]) ++
            (B.script B.! A.src "https://cdnjs.cloudflare.com/ajax/libs/mathjax/2.7.0/MathJax.js?config=TeX-AMS_CHTML-full,Safe,http://independentlearning.science/MathJax/config/local/local.js") "" ++
            (B.link B.! A.rel "stylesheet" 
                    B.! A.href "https://maxcdn.bootstrapcdn.com/bootswatch/3.3.7/flatly/bootstrap.min.css"
--                    B.! integrity "sha384-rwoIResjU2yc3z8GV/NPeZWAv56rSmLldC3R/AZzGRnGxQQKnKkoFVhFQhNUwEyJ" 
                    B.! crossorigin "anonymous") ++
            (B.link B.! A.rel "stylesheet"
                    B.! A.href "https://maxcdn.bootstrapcdn.com/font-awesome/4.7.0/css/font-awesome.min.css"
                    B.! crossorigin "anonymous") ++
            (B.style $ B.text $ unlines $ [ ".ils-logo-small, .ils-logo-container { height: 52px; width: 52px; display: inline-block; }"
                                        , "span.footnote:before { counter-increment: footnotecounter; content: counter(footnotecounter); position: relative; }"
                                        , "body { counter-reset: footnotecounter; }"
                                        ])) ++
  (B.body $ (B.toMarkup x) ++
            (B.script B.! A.src "https://code.jquery.com/jquery-3.1.1.slim.min.js"
--                       B.! integrity "sha384-A7FZj7v+d/sdmMqp/nOQwliLvUsJfDHW+k9Omg/a/EheAdgtzNs3hpfag6Ed950n"
                      B.! crossorigin "anonymous") "" ++
--             (B.script B.! A.src "https://cdnjs.cloudflare.com/ajax/libs/tether/1.4.0/js/tether.min.js"
--                       B.! integrity "sha384-DztdAPBWPRXSA/3eYEEUWrWCy7G5KFbe8fFjk5JAIxUYHKkDx6Qin1DkWx51bBrb"
--                       B.! crossorigin "anonymous") (text "") ++
            (B.script B.! A.src "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/js/bootstrap.min.js"
--                       B.! integrity "sha384-vBWWzlZJ8ea9aCX4pEW3rVHjgjt7zpkNpZk+02D9phzyeVkE+jo0ieGizqPLForn"
                      B.! crossorigin "anonymous") "")
  where
    -- integrity = B.customAttribute "integrity"
    crossorigin = B.customAttribute "crossorigin"


renderPretty :: B.ToMarkup a => a -> String
renderPretty x = 
  S.renderHtml $ 
  html x
