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

attributeT :: Text -> AttributeValue
attributeT = toValue

setidT :: SetID -> Text
setidT (SetID x) = idHashT x

instance ToValue SetID where
  toValue (SetID (IDHash x)) = toValue x

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
    B.h4 B.! A.class_ "blockTitle" B.! A.id (toValue x) $
    toMarkup x

instance ToMarkup MathText where
  toMarkup (MathText x) = toMarkup x

instance ToMarkup InlineMath where
  toMarkup (InlineMath x) = 
    B.span B.! A.class_ "inlineMath" $ 
    text "\\(" ++ toMarkup x ++ text "\\)"

instance ToMarkup CommentLine where
  toMarkup (CommentLine x) = textComment x

commonLinkT :: CommonLink -> Text
commonLinkT (CommonLink ((Protocol p), (URI u))) = p <> u

backT :: Back -> Text
backT (Back x) = idT x

outT :: Out -> Text
outT (Out x) = commonLinkT x

internalT :: Internal -> Text
internalT (Internal (IDHash x)) = x

instance ToMarkup LinkText where
  toMarkup (LinkText x) = toMarkup x

instance ToMarkup (LinkT Back) where
  toMarkup (LinkT {..}) = 
    B.a B.! A.class_ "backLink" B.! href (attributeT $ backT $ linkLocation) $ 
    lt
    where
      lt = maybe (toMarkup $ backT linkLocation) toMarkup linkText

instance ToMarkup (LinkT Out) where
  toMarkup (LinkT {..}) = 
    B.a B.! A.class_ "outLink" B.! href (attributeT $ outT $ linkLocation) $
    lt
    where
      lt = maybe (toMarkup $ outT linkLocation) toMarkup linkText

instance ToMarkup (LinkT Internal) where
  toMarkup (LinkT {..}) = 
    B.a B.! A.class_ "internalLink" B.! href (attributeT $ internalT $ linkLocation) $ 
    lt
    where
      lt = maybe (toMarkup $ internalT linkLocation) toMarkup linkText

instance ToMarkup Link where
  toMarkup (LBLink x) = toMarkup x
  toMarkup (LOLink x) = toMarkup x
  toMarkup (LILink x) = toMarkup x

instance ToMarkup (QTextT Bold) where
  toMarkup (QTextT {..}) = 
    B.strong B.! A.class_ "boldText" $ 
    toMarkup qtextText

instance ToMarkup (QTextT Italic) where
  toMarkup (QTextT {..}) = 
    B.em B.! A.class_ "italicText" $ 
    toMarkup qtextText

instance ToMarkup (QTextT Monospace) where
  toMarkup (QTextT {..}) = 
    B.code B.! A.class_ "monospaceText" $ 
    toMarkup qtextText

instance ToMarkup (QTextT Superscript) where
  toMarkup (QTextT {..}) = 
    B.sup B.! A.class_ "superscriptText" $ 
    toMarkup qtextText

instance ToMarkup (QTextT Subscript) where
  toMarkup (QTextT {..}) = 
    B.sub B.! A.class_ "subscriptText" $
    toMarkup qtextText

instance ToMarkup QText where
  toMarkup (QTBoldText x) = toMarkup x
  toMarkup (QTItalicText x) = toMarkup x
  toMarkup (QTMonospaceText x) = toMarkup x
  toMarkup (QTSuperscriptText x) = toMarkup x
  toMarkup (QTSubscriptText x) = toMarkup x

instance ToMarkup Footnote where
  toMarkup (Footnote {..}) = 
    B.div B.! A.class_ "footnoteContainer" $ concat $ 
    [ B.a B.! A.class_ "footnoteLink" B.! href "#" $ 
      B.sup B.! A.class_ "footnoteMarker" $
      B.text "fn"
    , (mLabel $ B.div B.! A.class_ "footnoteText") $
      toMarkup footnoteContent
    ]
    where
      --mHref = maybe "#" (\x -> (B.! A.id (attributeT $ setidT x))) footnoteID
      mLabel = maybe ClassyPrelude.id (\x -> (B.! A.id (toValue x))) footnoteID

instance ToMarkup FootnoteRef where
  toMarkup (FootnoteRef {..}) = 
    B.div B.! A.class_ "footnoteRefContainer" $
    B.a B.! A.class_ "footnoteRefLink" B.! href "#" $
    B.sup B.! A.class_ "footnoteRefMarker" $
    B.text "fnr"

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
    B.p $ concatMap toMarkup xs

instance ToMarkup Unordered where
  toMarkup _ = mempty

instance ToMarkup Ordered where
  toMarkup _ = mempty

instance ToMarkup Labelled where
  toMarkup (Labelled x) = 
    B.dt B.! A.class_ "labelledListLabel" $
    text x

mID :: Maybe SetID -> AttributeValue
mID = attributeT . (maybe "#" setidT)

instance ToMarkup (ListItem Unordered) where
  toMarkup (ListItem {..}) =
    mIDV listItemID $
    B.li B.! A.class_ "unorderedListItem" $
    toMarkup listItemContents

instance ToMarkup (ListItem Ordered) where
  toMarkup (ListItem {..}) =
    mIDV listItemID $ 
    B.li B.! A.class_ "orderedListItem" $
    toMarkup listItemContents

instance ToMarkup (ListItem Labelled) where
  toMarkup (ListItem {..}) =
    toMarkup listItemLabel ++
    (mIDV listItemID $
     B.dd B.! A.class_ "labelledListItem" $
     toMarkup listItemContents)

mIDV :: Maybe SetID -> Html -> Html
mIDV id_ = maybe ClassyPrelude.id (\x -> (B.! A.id (toValue x))) id_

instance ToMarkup (ListT Unordered) where
  toMarkup (ListT {..}) =
    mIDV listID $
    B.ul B.! A.class_ "unorderedList" $
    concatMap toMarkup listItems

instance ToMarkup (ListT Ordered) where
  toMarkup (ListT {..}) =
    mIDV listID $
    B.ol B.! A.class_ "orderedList" $
    concatMap toMarkup listItems

instance ToMarkup (ListT Labelled) where
  toMarkup (ListT {..}) =
    mIDV listID $
    B.dl B.! A.class_ "labelledList" $
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

instance ToMarkup BibliographyContent where
  toMarkup (BibliographyContent x) = 
    B.li B.! A.class_ "bibliographyItem" $
    toMarkup x

instance ToMarkup PrerexItem where
  toMarkup (PrerexItem x) = 
    B.li B.! A.class_ "prerexItem" $ 
    B.a B.! A.class_ "prerexItemLink" B.! (A.href $ attributeT $ idPathT x) $ 
    toMarkup $ idPathT x

instance ToMarkup (BlockType a) where
  toMarkup (BlockType x) = 
    B.span B.! A.class_ "blockType" $ toMarkup x

instance ToMarkup Prerex where
  toMarkup (Prerex xs) =
    B.div B.! A.class_ "prerexContents" $
    B.ol B.! A.class_ "prerexList" $
    concatMap toMarkup xs

instance ToMarkup Math where
  toMarkup (Math x) = 
    B.div B.! A.class_ "mathContents" $
    B.div B.! A.class_ "displayMath" $
    text "\\[" ++ toMarkup x ++ text "\\]"

instance ToMarkup EqnArray where
  toMarkup  (EqnArray xs) =
    B.div B.! A.class_ "eqnArrayContents" $
    B.div B.! A.class_ "displayMath" $
    text "$$\\begin{eqnarray}\n" ++ 
    (concatMap (\x -> toMarkup $ x <> "\\\\\n") xs) ++ 
    text "\\end{eqnarray}$$"

instance ToMarkup Theorem where
  toMarkup (Theorem xs) =
    B.div B.! A.class_ "theoremContents" $
    B.section B.! A.class_ "theorem" $
    concatMap toMarkup xs
    
instance ToMarkup Proof where
  toMarkup (Proof xs) =
    B.div B.! A.class_ "proofContents" $
    B.section B.! A.class_ "proof" $
    concatMap toMarkup xs

instance ToMarkup Quote where
  toMarkup (Quote x) =
    B.div B.! A.class_ "quoteContents" $
    B.blockquote B.! A.class_ "quote" $ 
    toMarkup x

instance ToMarkup Code where
  toMarkup (Code x) =
    B.div B.! A.class_ "codeContents" $
    B.code B.! A.class_ "code" $
    toMarkup x

instance ToValue Internal where
  toValue = attributeT . internalT

instance ToValue Out where
  toValue = attributeT . outT

instance ToValue Back where
  toValue = attributeT . backT

instance ToMarkup Image where
  toMarkup (Image (LinkT {..})) = 
    B.div B.! A.class_ "imageContents" $
    mLabel $ B.img B.! A.class_ "imageBlockImage" B.! A.src (toValue linkLocation)
    where
      mLabel = maybe ClassyPrelude.id (\(LinkText x) -> (B.! A.alt (textValue x))) linkText

instance ToMarkup Video where
  toMarkup (Video (LinkT {..})) =
    B.div B.! A.class_ "videoContents" $
    mLabel $ B.video B.! A.class_ "videoBlockVideo" B.! A.controls "true" B.! A.src (toValue linkLocation) $
    text ""
    where
      mLabel = maybe ClassyPrelude.id (\(LinkText x) -> (B.! A.alt (textValue x))) linkText

instance ToMarkup YouTube where
  toMarkup (YouTube (LinkT {..})) = 
    B.div B.! A.class_ "youTubeContents" $
    mLabel $ video B.! A.class_ "youTubeBlockYouTube" B.! A.src (toValue linkLocation) $ 
    text ""
    where
      mLabel = maybe ClassyPrelude.id (\(LinkText x) -> (B.! A.alt (textValue x))) linkText

instance ToMarkup Aside where
  toMarkup (Aside (mpb, xs)) = 
    B.div B.! A.class_ "asideContents" $
    B.aside B.! A.class_ "aside" $
    (toMarkup pb) B.! A.class_ "asidePrerex" ++
    (B.section B.! A.class_ "asideSection" $ 
     concatMap toMarkup xs)
    where
      pb = maybe "" toMarkup mpb

instance ToMarkup Admonition where
  toMarkup (Admonition xs) =
    B.div B.! A.class_ "admonitionContents" $
    B.aside B.! A.class_ "admonition" $
    concatMap toMarkup xs

instance ToMarkup Sidebar where
  toMarkup (Sidebar xs) =
    B.div B.! A.class_ "sidebarContents" $
    B.aside B.! A.class_ "sidebar" $
    concatMap toMarkup xs

instance ToMarkup Example where
  toMarkup (Example xs) =
    B.div B.! A.class_ "exampleContents" $
    B.section B.! A.class_ "example" $
    concatMap toMarkup xs

instance ToMarkup Exercise where
  toMarkup (Exercise xs) =
    B.div B.! A.class_ "exerciseContents" $
    B.section B.! A.class_ "exercise" $
    concatMap toMarkup xs

instance ToMarkup Bibliography where
  toMarkup (Bibliography xs) =
    B.div B.! A.class_ "bibliographyContents" $
    B.section B.! A.class_ "bibliography" $
    B.ol B.! A.class_ "bibliographyList" $
    concatMap toMarkup xs

instance ToMarkup bType =>
  ToMarkup (BlockT bType) where
  toMarkup (BlockT {..}) =
    mIDV blockID $ 
    B.div B.! A.class_ "block" B.! A.class_ (bType ++ "Block") $
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
    B.h1 B.! A.class_ "title" $
    toMarkup x

instance ToMarkup Section where
  toMarkup (Section x) =
    B.h2 B.! A.class_ "sectionTitle" $
    toMarkup x

instance ToMarkup Subsection where
  toMarkup (Subsection x) =
    B.h3 B.! A.class_ "subsectionTitle" $
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
    B.article B.! A.class_ "idoc" $
    toMarkup docTitle ++
    toMarkup docPrerex ++
    concatMap toMarkup docContents

renderPretty :: Doc -> String
renderPretty x = 
  P.renderHtml $ 
  B.docTypeHtml $
  (B.head $ B.script B.! A.async "true" 
                     B.! A.type_ "text/javascript" 
                     B.! A.src "https://cdnjs.cloudflare.com/ajax/libs/mathjax/2.7.0/MathJax.js?config=TeX-AMS_CHTML" $
    text "") ++
  (B.body $
   toMarkup $
   x)
  
