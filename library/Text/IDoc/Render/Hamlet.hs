-- | Hamlet.hs
-- Author: Matt Walker
-- License: https://opensource.org/licenses/BSD-2-Clause
-- Created: Apr 02, 2017
-- Summary: 

{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Text.IDoc.Render.Hamlet where

import ClassyPrelude
import Text.IDoc.Parse
import Text.Hamlet
import Text.Blaze.Html5 as B
import Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html.Renderer.Pretty as P
import Text.Blaze.Html.Renderer.String as S

attributeT :: Text -> AttributeValue
attributeT = toValue

setidT :: SetID -> Text
setidT (SetID x) = idHashT x

instance ToValue SetID where
  toValue (SetID (IDHash x)) = toValue x

instance ToValue IDPathComponent where
  toValue (IDPathComponent x) = toValue x

instance ToValue IDPath where
  toValue (IDPath xs) =
    concatMap toValue $ intersperse (IDPathComponent "-") xs

idPathT :: IDPath -> Text
idPathT x = concat $ 
            cons "/" $ 
            intersperse "/" $ 
            fmap (\(IDPathComponent y) -> y) $
            (\(IDPath xs) -> xs) $
            x

idHashT :: IDHash -> Text
idHashT (IDHash x) = "#" <> x

idT :: ID -> Text
idT (ID {..}) = idPathT idPath <> idHashT idHash

instance ToMarkup BlockTitle where
  toMarkup (BlockTitle x) = 
    B.h4 B.! A.class_ "idoc-block-title" 
         B.! A.class_ "h4"
         B.! A.id (toValue x) $
    toMarkup x

instance ToMarkup MathText where
  toMarkup (MathText x) = toMarkup x

instance ToMarkup InlineMath where
  toMarkup (InlineMath x) = 
    B.span B.! A.class_ "idoc-inline-math" $
    text "\\(" ++ toMarkup x ++ text "\\)"

instance ToMarkup CommentLine where
  toMarkup (CommentLine x) = textComment x

commonLinkT :: CommonLink -> Text
commonLinkT (CommonLink ((Protocol p), (URI u))) = p <> "://" <> u

backT :: Back -> Text
backT (Back x) = idT x

outT :: Out -> Text
outT (Out x) = commonLinkT x

internalT :: Internal -> Text
internalT (Internal (IDHash x)) = "#" <> x

instance ToMarkup AttrValue where
  toMarkup (AttrValue x) = toMarkup x

instance ToMarkup LinkText where
  toMarkup (LinkText x) = toMarkup x

instance ToMarkup (LinkT Back) where
  toMarkup (LinkT {..}) = 
    B.a B.! A.class_ "idoc-back-link" 
        B.! B.dataAttribute "toggle" "tooltip"
        B.! href (attributeT $ backT $ linkLocation) $ 
    lt
    where
      lt = maybe (toMarkup $ backT linkLocation) toMarkup linkText

instance ToMarkup (LinkT Out) where
  toMarkup (LinkT {..}) = 
    B.a B.! A.class_ "idoc-out-link"
        B.! B.dataAttribute "toggle" "tooltip"
        B.! href (attributeT $ outT $ linkLocation) $
    lt
    where
      lt = maybe (toMarkup $ outT linkLocation) toMarkup linkText

instance ToMarkup (LinkT Internal) where
  toMarkup (LinkT {..}) = 
    B.a B.! A.class_ "idoc-internal-link" 
        B.! B.dataAttribute "toggle" "tooltip"
        B.! href (attributeT $ internalT $ linkLocation) $ 
    lt
    where
      lt = maybe (toMarkup $ internalT linkLocation) toMarkup linkText

instance ToMarkup Link where
  toMarkup (LBLink x) = toMarkup x
  toMarkup (LOLink x) = toMarkup x
  toMarkup (LILink x) = toMarkup x

instance ToMarkup (QTextT Bold) where
  toMarkup (QTextT {..}) = 
    B.strong B.! A.class_ "idoc-strong-text" $ 
    toMarkup qtextText

instance ToMarkup (QTextT Italic) where
  toMarkup (QTextT {..}) = 
    B.em B.! A.class_ "idoc-emph-text" $ 
    toMarkup qtextText

instance ToMarkup (QTextT Monospace) where
  toMarkup (QTextT {..}) = 
    B.code B.! A.class_ "idoc-monospace-text" $ 
    toMarkup qtextText

instance ToMarkup (QTextT Superscript) where
  toMarkup (QTextT {..}) = 
    B.sup B.! A.class_ "idoc-superscript-text" $ 
    toMarkup qtextText

instance ToMarkup (QTextT Subscript) where
  toMarkup (QTextT {..}) = 
    B.sub B.! A.class_ "idoc-subscript-text" $
    toMarkup qtextText

instance ToMarkup QText where
  toMarkup (QTBoldText x) = toMarkup x
  toMarkup (QTItalicText x) = toMarkup x
  toMarkup (QTMonospaceText x) = toMarkup x
  toMarkup (QTSuperscriptText x) = toMarkup x
  toMarkup (QTSubscriptText x) = toMarkup x

instance ToMarkup Footnote where
  toMarkup (Footnote {..}) = 
    B.div B.! A.class_ "idoc-footnote-contents" 
          B.! A.class_ "container"
    $ concat $
    [ B.cite B.! A.class_ "idoc-footnote-cite" $ 
      B.a B.! A.class_ "idoc-footnote-link" 
          B.! href "#" $ 
      B.sup B.! A.class_ "idoc-footnote-marker"
            B.! B.dataAttribute "toggle" "tooltip" $
      B.text "note"
    , mIDV footnoteID $ 
      B.footer B.! A.class_ "idoc-footnote-text" $
      toMarkup footnoteContent
    ]

instance ToMarkup FootnoteRef where
  toMarkup (FootnoteRef {..}) = 
    B.div B.! A.class_ "idoc-footnote-ref-contents" $
    B.cite B.! A.class_ "idoc-footnote-ref-cite" $ 
    B.a B.! A.class_ "idoc-footnote-ref-link" 
        B.! B.dataAttribute "toggle" "tooltip"
        B.! href "#" $
    B.sup B.! A.class_ "idoc-footnote-ref-marker" $
    B.text "note"

instance ToMarkup PlainText where
  toMarkup (PlainText x) = toMarkup x

instance ToMarkup SimpleContent where
  toMarkup (SCInlineMath x) = toMarkup x
  toMarkup (SCLink x) = toMarkup x
  toMarkup (SCFootnote x) = toMarkup x
  toMarkup (SCFootnoteRef x) = toMarkup x
  toMarkup (SCQText x) = toMarkup x

instance ToMarkup ParagraphContent where
  toMarkup (PCSimpleContent x) = toMarkup x
  toMarkup (PCPlainText x) = toMarkup x

instance ToMarkup Paragraph where
  toMarkup (Paragraph (xs, _)) = 
    B.p B.! A.class_ "idoc-paragraph" $
    concatMap toMarkup xs

instance ToMarkup Unordered where
  toMarkup _ = mempty

instance ToMarkup Ordered where
  toMarkup _ = mempty

instance ToMarkup Labelled where
  toMarkup (Labelled x) = 
    B.dt B.! A.class_ "idoc-labelled-list-label"
         B.! A.class_ "list-group-item-heading" $
    text x

mID :: Maybe SetID -> AttributeValue
mID = attributeT . (maybe "#" setidT)

instance ToMarkup (ListItem Unordered) where
  toMarkup (ListItem {..}) =
    mIDV listItemID $
    B.li B.! A.class_ "idoc-unordered-list-item"
         B.! A.class_ "list-group-item" $
    toMarkup listItemContents

instance ToMarkup (ListItem Ordered) where
  toMarkup (ListItem {..}) =
    mIDV listItemID $ 
    B.li B.! A.class_ "idoc-ordered-list-item"
         B.! A.class_ "list-group-item" $
    toMarkup listItemContents

instance ToMarkup (ListItem Labelled) where
  toMarkup (ListItem {..}) =
    toMarkup listItemLabel ++
    (mIDV listItemID $
     B.dd B.! A.class_ "idoc-labelled-list-item"
          B.! A.class_ "list-group-item-info"$
     toMarkup listItemContents)

mIDV :: Maybe SetID -> Html -> Html
mIDV id_ = maybe ClassyPrelude.id (\x -> (B.! A.id (toValue x))) id_

instance ToMarkup (ListT Unordered) where
  toMarkup (ListT {..}) =
    mIDV listID $
    B.ul B.! A.class_ "idoc-unordered-list"
         B.! A.class_ "list-unstyled"
         B.! A.class_ "list-group" $
    concatMap toMarkup listItems

instance ToMarkup (ListT Ordered) where
  toMarkup (ListT {..}) =
    mIDV listID $
    B.ol B.! A.class_ "idoc-ordered-list" 
         B.! A.class_ "list-unstyled"
         B.! A.class_ "list-group" $
    concatMap toMarkup listItems

instance ToMarkup (ListT Labelled) where
  toMarkup (ListT {..}) =
    mIDV listID $
    B.dl B.! A.class_ "idoc-labelled-list"
         B.! A.class_ "list-unstyled"
         B.! A.class_ "dl-horizontal" $
    concatMap toMarkup listItems

instance ToMarkup List where
  toMarkup (LUList x) = toMarkup x
  toMarkup (LOList x) = toMarkup x
  toMarkup (LLList x) = toMarkup x

instance ToMarkup ImageLink where
  toMarkup (ImageLink x) = toMarkup x

instance ToMarkup VideoLink where
  toMarkup (VideoLink x) = toMarkup x

instance ToMarkup YouTubeLink where
  toMarkup (YouTubeLink x) = toMarkup x

instance ToMarkup BibliographyItem where
  toMarkup (BibliographyItem x) = 
    B.li B.! A.class_ "idoc-bibliography-item" $
    toMarkup x

instance ToMarkup PrerexItem where
  toMarkup (PrerexItem x) = 
    B.div B.! A.class_ "idoc-prerex-item panel panel-info" $
          (B.div B.! A.class_ "panel-heading" $
                 B.h3 B.! A.class_ "panel-title" $
                      B.a B.! B.dataAttribute "toggle" "collapse"
                          B.! href ("#" ++ toValue x) $
                          toMarkup $ idPathT x) ++
          (B.div B.! A.class_ "panel-collapse collapse"
                 B.! A.id (toValue x) $
                 (B.div B.! A.class_ "idoc-prerex-item-content panel-body" $
                        "Default Prerex Content Description.") ++
                 (B.div B.! A.class_ "idoc-prerex-item-footer panel-footer" $
                        B.a B.! A.class_ "idoc-prerex-item-link"
                            B.! A.href (toValue $ idPathT x) $
                            toMarkup $ idPathT x))

instance ToMarkup (BlockType a) where
  toMarkup (BlockType x) = 
    B.span B.! A.class_ "idoc-block-type" $ toMarkup x

instance ToMarkup PrerexContents where
  toMarkup (PrerexContents xs) =
    B.div B.! A.class_ "idoc-prerex-list panel-group" $
    concatMap toMarkup xs
instance ToMarkup Prerex where toMarkup (Prerex x _) = toMarkup x

instance ToMarkup MathContents where
  toMarkup (MathContents x) = 
    B.div B.! A.class_ "idoc-math-contents center-block flex" $
    B.div B.! A.class_ "idoc-display-math" $
    text "\\[" ++ toMarkup x ++ text "\\]"
instance ToMarkup Math where toMarkup (Math x _) = toMarkup x

instance ToMarkup EqnArrayContents where
  toMarkup (EqnArrayContents xs) =
    B.div B.! A.class_ "idoc-eqn-array-contents"
          B.! A.class_ "bg-info"
          B.! A.class_ "center-block" $
    B.div B.! A.class_ "idoc-display-math" $
    text "$$\\begin{eqnarray}\n" ++ 
    (concatMap (\x -> toMarkup $ x <> "\\\\\n") xs) ++ 
    text "\\end{eqnarray}$$"
instance ToMarkup EqnArray where toMarkup (EqnArray x _) = toMarkup x

instance ToMarkup TheoremContents where
  toMarkup (TheoremContents xs) =
    B.div B.! A.class_ "idoc-theorem-contents" $
    B.section B.! A.class_ "idoc-theorem" $
    concatMap toMarkup xs
instance ToMarkup Theorem where toMarkup (Theorem x _) = toMarkup x
    
instance ToMarkup ProofContents where
  toMarkup (ProofContents xs) =
    B.div B.! A.class_ "idoc-proof-contents" $
    B.section B.! A.class_ "idoc-proof" $
    concatMap toMarkup xs
instance ToMarkup Proof where toMarkup (Proof x _) = toMarkup x

instance ToMarkup Quote where
  toMarkup (Quote (QuoteContents x) (AttrList al)) =
    B.div B.! A.class_ "idoc-blockquote-contents" $
          B.blockquote B.! A.class_ "idoc-blockquote" $
                       mCite $ toMarkup x
    where
      mCite = maybe ClassyPrelude.id (\mAuthor -> 
                                        maybe ClassyPrelude.id 
                                        (\author -> (++ " " ++ (B.cite B.! A.class_ "idoc-blockquote-author" $
                                                                 toMarkup author))) mAuthor) (lookup "author" al)

instance ToMarkup CodeContents where
  toMarkup (CodeContents x) =
    B.div B.! A.class_ "idoc-code-contents" $
    B.code B.! A.class_ "idoc-code" $
    toMarkup x
instance ToMarkup Code where toMarkup (Code x _) = toMarkup x

instance ToValue Internal where
  toValue = attributeT . internalT

instance ToValue Out where
  toValue = attributeT . outT

instance ToValue Back where
  toValue = attributeT . backT

instance ToMarkup ImageContents where
  toMarkup (ImageContents (LinkT {..})) = 
    B.div B.! A.class_ "idoc-image-contents" $
    mLabel $ B.img B.! A.class_ "idoc-image-block-image" B.! A.src (toValue linkLocation)
    where
      mLabel = maybe ClassyPrelude.id (\(LinkText x) -> (B.! A.alt (textValue x))) linkText
instance ToMarkup Image where toMarkup (Image x _) = toMarkup x

instance ToMarkup VideoContents where
  toMarkup (VideoContents (LinkT {..})) =
    B.div B.! A.class_ "idoc-video-contents" $
    mLabel $ B.video B.! A.class_ "idoc-video-block-video" B.! A.controls "true" B.! A.src (toValue linkLocation) $
    text ""
    where
      mLabel = maybe ClassyPrelude.id (\(LinkText x) -> (B.! A.alt (textValue x))) linkText
instance ToMarkup Video where toMarkup (Video x _) = toMarkup x

instance ToMarkup YouTubeContents where
  toMarkup (YouTubeContents (LinkT {..})) = 
    B.div B.! A.class_ "idoc-youtube-contents" $
    mLabel $ video B.! A.class_ "idoc-youtube-block-youtube" B.! A.src (toValue linkLocation) $ 
    text ""
    where
      mLabel = maybe ClassyPrelude.id (\(LinkText x) -> (B.! A.alt (textValue x))) linkText
instance ToMarkup YouTube where toMarkup (YouTube x _) = toMarkup x

instance ToMarkup AsideContents where
  toMarkup (AsideContents (mpb, xs)) = 
    B.div B.! A.class_ "idoc-aside-contents" $
    B.aside B.! A.class_ "idoc-aside" $
    (toMarkup pb) B.! A.class_ "idoc-aside-prerex" ++
    (B.section B.! A.class_ "idoc-aside-section" $ 
     concatMap toMarkup xs)
    where
      pb = maybe "" toMarkup mpb
instance ToMarkup Aside where toMarkup (Aside x _) = toMarkup x

instance ToMarkup AdmonitionContents where
  toMarkup (AdmonitionContents xs) =
    B.div B.! A.class_ "idoc-admonition-contents" $
    B.aside B.! A.class_ "idoc-admonition" $
    concatMap toMarkup xs
instance ToMarkup Admonition where toMarkup (Admonition x _) = toMarkup x

instance ToMarkup SidebarContents where
  toMarkup (SidebarContents xs) =
    B.div B.! A.class_ "idoc-sidebar-contents" $
    B.aside B.! A.class_ "idoc-sidebar" $
    concatMap toMarkup xs
instance ToMarkup Sidebar where toMarkup (Sidebar x _) = toMarkup x

instance ToMarkup ExampleContents where
  toMarkup (ExampleContents xs) =
    B.div B.! A.class_ "idoc-example-contents" $
    B.section B.! A.class_ "idoc-example" $
    concatMap toMarkup xs
instance ToMarkup Example where toMarkup (Example x _) = toMarkup x

instance ToMarkup ExerciseContents where
  toMarkup (ExerciseContents xs) =
    B.div B.! A.class_ "idoc-exercise-contents" $
    B.section B.! A.class_ "idoc-exercise" $
    concatMap toMarkup xs
instance ToMarkup Exercise where toMarkup (Exercise x _) = toMarkup x

instance ToMarkup BibliographyContents where
  toMarkup (BibliographyContents xs) =
    B.div B.! A.class_ "idoc-bibliography-contents" $
          B.section B.! A.class_ "idoc-bibliography" $
                    B.ol B.! A.class_ "idoc-bibliography-list" $
                         concatMap toMarkup xs
instance ToMarkup Bibliography where toMarkup (Bibliography x _) = toMarkup x

instance ToMarkup bType =>
  ToMarkup (BlockT bType) where
  toMarkup (BlockT {..}) =
    mIDV blockID $ 
    B.div B.! A.class_ "idoc-block" B.! A.class_ ("idoc-" ++ bType ++ "-block") $
    mBTitle ++
    toMarkup blockContents
    where
      bType = case blockType of (BlockType bt) -> toValue bt
      mBTitle = maybe "" toMarkup blockTitle

instance ToMarkup Block where
  toMarkup (BMathBlock x) = toMarkup x
  toMarkup (BEqnArrayBlock x) = toMarkup x
  toMarkup (BTheoremBlock x) = toMarkup x
  toMarkup (BProofBlock x) = toMarkup x
  toMarkup (BPrerexBlock x) = toMarkup x
  toMarkup (BQuoteBlock x) = toMarkup x
  toMarkup (BCodeBlock x) = toMarkup x
  toMarkup (BImageBlock x) = toMarkup x
  toMarkup (BVideoBlock x) = toMarkup x
  toMarkup (BAdmonitionBlock x) = toMarkup x
  toMarkup (BAsideBlock x) = toMarkup x
  toMarkup (BYouTubeBlock x) = toMarkup x
  toMarkup (BSidebarBlock x) = toMarkup x
  toMarkup (BExampleBlock x) = toMarkup x
  toMarkup (BExerciseBlock x) = toMarkup x
  toMarkup (BBibliographyBlock x) = toMarkup x

instance ToMarkup Title where
  toMarkup (Title x) =
    B.h1 B.! A.class_ "idoc-title" $
    toMarkup x

instance ToMarkup Section where
  toMarkup (Section x) =
    B.h2 B.! A.class_ "idoc-section-title"
         B.! A.id (toValue x) $
    toMarkup x

instance ToMarkup Subsection where
  toMarkup (Subsection x) =
    B.h3 B.! A.class_ "idoc-subsection-title"
         B.! A.id (toValue x) $
    toMarkup x

instance ToMarkup hType =>
  ToMarkup (Heading hType) where
  toMarkup (Heading {..}) =
    mIDV hID $ toMarkup hContents

instance ToMarkup ComplexContent where
  toMarkup (CCBlock x) = toMarkup x
  toMarkup (CCList x) = toMarkup x
  toMarkup (CCParagraph x) = toMarkup x

instance ToMarkup TopLevelContent where
  toMarkup (TLCSubsectionHeading x) = toMarkup x
  toMarkup (TLCSectionHeading x) = toMarkup x
  toMarkup (TLCCommentLine x) = toMarkup x
  toMarkup (TLCComplexContent x) = toMarkup x

instance ToMarkup Doc where
  toMarkup (Doc {..}) = 
    B.article B.! A.class_ "idoc-doc container" $
    toMarkup docTitle ++
    toMarkup docPrerex ++
    concatMap toMarkup docContents

renderPretty :: Doc -> String
renderPretty x = 
  S.renderHtml $ 
  B.docTypeHtml $ B.html $
  (B.head $ (B.meta B.! A.charset "utf-8") ++
            (B.meta B.! A.name "viewport"
                    B.! A.content "width=device-width, initial-scale=1, shrink-to-fit=no") ++
            (B.script B.! A.async "true" 
                      B.! A.src "https://cdnjs.cloudflare.com/ajax/libs/mathjax/2.7.0/MathJax.js?config=TeX-AMS_CHTML") (text "") ++
            (B.link B.! A.rel "stylesheet" 
                    B.! A.href "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/css/bootstrap.min.css"
--                    B.! integrity "sha384-rwoIResjU2yc3z8GV/NPeZWAv56rSmLldC3R/AZzGRnGxQQKnKkoFVhFQhNUwEyJ" 
                    B.! crossorigin "anonymous")) ++
   (B.body $ (toMarkup x) ++
             (B.script B.! A.src "https://code.jquery.com/jquery-3.1.1.slim.min.js"
--                       B.! integrity "sha384-A7FZj7v+d/sdmMqp/nOQwliLvUsJfDHW+k9Omg/a/EheAdgtzNs3hpfag6Ed950n"
                       B.! crossorigin "anonymous") (text "") ++
--             (B.script B.! A.src "https://cdnjs.cloudflare.com/ajax/libs/tether/1.4.0/js/tether.min.js"
--                       B.! integrity "sha384-DztdAPBWPRXSA/3eYEEUWrWCy7G5KFbe8fFjk5JAIxUYHKkDx6Qin1DkWx51bBrb"
--                       B.! crossorigin "anonymous") (text "") ++
             (B.script B.! A.src "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/js/bootstrap.min.js"
--                       B.! integrity "sha384-vBWWzlZJ8ea9aCX4pEW3rVHjgjt7zpkNpZk+02D9phzyeVkE+jo0ieGizqPLForn"
                       B.! crossorigin "anonymous") (text ""))
  where
    integrity = B.customAttribute "integrity"
    crossorigin = B.customAttribute "crossorigin"
  
