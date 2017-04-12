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
import Text.Blaze.Html.Renderer.String as S

attributeT :: Text -> AttributeValue
attributeT = toValue

setidT :: SetID -> Text
setidT (SetID x) = idHashT x

instance ToValue SetID where
  toValue (SetID (IDHash x)) = toValue x

instance ToValue (IDPathComponent a) where
  toValue (IDPathComponent x) = toValue x

instance ToValue (IDPath a) where
  toValue (IDPath xs) =
    concatMap toValue $ intersperse (IDPathComponent "-") xs

idPathT :: (IDPath a) -> Text
idPathT x = concat $ 
            cons "/" $ 
            intersperse "/" $ 
            fmap (\(IDPathComponent y) -> y) $
            (\(IDPath xs) -> xs) $
            x

idHashT :: (IDHash a) -> Text
idHashT (IDHash x) = "#" <> x

idT :: (ID a) -> Text
idT (ID {..}) = idPathT idPath <> idHashT idHash

instance ToMarkup BlockHeading where
  toMarkup (BlockHeading x) = 
    B.h4 B.! A.class_ "idoc-block-title" 
         B.! A.class_ "h4" $
--         B.! A.id (toValue x) $
    toMarkup x

instance ToMarkup InlineMath where
  toMarkup (InlineMath x) = 
    B.span B.! A.class_ "idoc-inline-math" $
    text "\\(" ++ toMarkup x ++ text "\\)"

-- instance ToMarkup CommentLine where
--   toMarkup (CommentLine x) = textComment x

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

instance LineLike a => ToMarkup (QTextT a Bold) where
  toMarkup (QTextT {..}) = 
    B.strong B.! A.class_ "idoc-strong-text" $ 
    toMarkup qtextText

instance LineLike a => ToMarkup (QTextT a Italic) where
  toMarkup (QTextT {..}) = 
    B.em B.! A.class_ "idoc-emph-text" $ 
    toMarkup qtextText

instance LineLike a => ToMarkup (QTextT a Monospace) where
  toMarkup (QTextT {..}) = 
    B.code B.! A.class_ "idoc-monospace-text" $ 
    toMarkup qtextText

instance LineLike a => ToMarkup (QTextT a Superscript) where
  toMarkup (QTextT {..}) = 
    B.sup B.! A.class_ "idoc-superscript-text" $ 
    toMarkup qtextText

instance LineLike a => ToMarkup (QTextT a Subscript) where
  toMarkup (QTextT {..}) = 
    B.sub B.! A.class_ "idoc-subscript-text" $
    toMarkup qtextText

instance LineLike a => ToMarkup (QTextT a Quoted) where
  toMarkup (QTextT {..}) = 
    B.span B.! A.class_ "idoc-quoted-text" $
    toMarkup ("\"" <> qtextText <> "\"")

instance LineLike a => ToMarkup (QText a) where
  toMarkup (QTBoldText x) = toMarkup x
  toMarkup (QTItalicText x) = toMarkup x
  toMarkup (QTMonospaceText x) = toMarkup x
  toMarkup (QTSuperscriptText x) = toMarkup x
  toMarkup (QTSubscriptText x) = toMarkup x
  toMarkup (QTQuotedText x) = toMarkup x

instance ToMarkup Footnote where
  toMarkup (Footnote {..}) = 
    B.div B.! A.class_ "idoc-footnote-contents" $
          concat $
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

instance LineLike a => ToMarkup (PlainText a) where
  toMarkup (PlainText x) = toMarkup x

instance LineLike a => ToMarkup (SimpleContent a) where
  toMarkup (SCInlineMath x) = toMarkup x
  toMarkup (SCLink x) = toMarkup x
  toMarkup (SCFootnote x) = toMarkup x
  toMarkup (SCFootnoteRef x) = toMarkup x
  toMarkup (SCQText x) = toMarkup x

instance LineLike a => ToMarkup (ParagraphContent a) where
  toMarkup (PCSimpleContent x) = toMarkup x
  toMarkup (PCPlainText x) = toMarkup x

instance LineLike a => ToMarkup (Paragraph a) where
  toMarkup (Paragraph xs pid) = 
    spanOrP $ concatMap toMarkup xs
    where
      spanOrP = 
        let (LikeLine x :: LikeLine a) = lineLike
        in
          if x then
            B.span B.! A.class_ "idoc-paragraph-line-like"
          else
            B.p B.! A.class_ "idoc-paragraph"

instance ToMarkup Unordered where
  toMarkup _ = mempty

instance ToMarkup Ordered where
  toMarkup _ = mempty

instance ToMarkup Labelled where
  toMarkup (Labelled x) = 
    B.dt B.! A.class_ "idoc-labelled-list-label list-group-item-heading" $
         toMarkup x

mID :: Maybe SetID -> AttributeValue
mID = attributeT . (maybe "#" setidT)

instance LineLike a => ToMarkup (ListItem a Unordered) where
  toMarkup (ListItem {..}) =
    B.li B.! A.class_ "idoc-unordered-list-item list-group-item" $
         toMarkup listItemContents

instance LineLike a => ToMarkup (ListItem a Ordered) where
  toMarkup (ListItem {..}) =
    B.li B.! A.class_ "idoc-ordered-list-item list-group-item" $
         toMarkup listItemContents

instance LineLike a => ToMarkup (ListItem a Labelled) where
  toMarkup (ListItem {..}) =
    toMarkup listItemLabel ++
     (B.dd B.! A.class_ "idoc-labelled-list-item list-group-item" $
           toMarkup listItemContents)

mIDV :: Maybe SetID -> Html -> Html
mIDV id_ = maybe ClassyPrelude.id (\x -> (B.! A.id (toValue x))) id_

instance LineLike a => ToMarkup (ListT a Unordered) where
  toMarkup (ListT {..}) =
    B.ul B.! A.class_ "idoc-unordered-list list-group" $
         concatMap toMarkup listItems

instance LineLike a => ToMarkup (ListT a Ordered) where
  toMarkup (ListT {..}) =
    B.ol B.! A.class_ "idoc-ordered-list list-group" $
         concatMap toMarkup listItems

instance LineLike a => ToMarkup (ListT a Labelled) where
  toMarkup (ListT {..}) =
    B.dl B.! A.class_ "idoc-labelled-list dl-horizontal" $
         concatMap toMarkup listItems

instance LineLike a => ToMarkup (List a) where
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
    B.span B.! A.class_ "idoc-block-type" $ 
           toMarkup x

instance ToMarkup Prerex where
  toMarkup (Prerex xs) =
    B.div B.! A.class_ "idoc-prerex-list panel-group" $
          concatMap toMarkup xs

instance ToMarkup Math where
  toMarkup (Math x) = 
    B.div B.! A.class_ "idoc-math-contents center-block flex" $
          B.div B.! A.class_ "idoc-display-math" $
                text "\\[" ++ toMarkup x ++ text "\\]"

instance ToMarkup EqnArray where
  toMarkup (EqnArray xs) =
    B.div B.! A.class_ "idoc-eqn-array-contents bg-info center-block" $
          B.div B.! A.class_ "idoc-display-math" $
                text "$$\\begin{eqnarray}\n" ++ 
                (concatMap (\(Equation x) -> toMarkup $ x <> "\\\\\n") xs) ++ 
                text "\\end{eqnarray}$$"

instance ToMarkup Theorem where
  toMarkup (Theorem x) =
    B.div B.! A.class_ "idoc-theorem-contents" $
          toMarkup x
    
instance ToMarkup Proof where
  toMarkup (Proof x) =
    B.div B.! A.class_ "idoc-proof-contents" $
          toMarkup x

quoteBlockMarkup :: BlockT Quote -> Html
quoteBlockMarkup (BlockT {..}) =
  B.div B.! A.class_ "idoc-blockquote-contents" $
        B.blockquote B.! A.class_ "idoc-blockquote" $
                     mCite $ toMarkup $ (\case (Quote x) -> x) blockContents
  where
    mCite = maybe ClassyPrelude.id (\mAuthor -> 
                                       maybe ClassyPrelude.id 
                                       (\author -> (++ " " ++ (B.cite B.! A.class_ "idoc-blockquote-author" $
                                                                toMarkup author))) mAuthor) (lookup "author" $ (\case (AttrMap x) -> x) blockAttrs)

instance ToMarkup Code where
  toMarkup (Code x) =
    B.div B.! A.class_ "idoc-code-contents" $
          B.code B.! A.class_ "idoc-code" $
                 toMarkup x

instance ToValue Internal where
  toValue = attributeT . internalT

instance ToValue Out where
  toValue = attributeT . outT

instance ToValue Back where
  toValue = attributeT . backT

instance ToMarkup Image where
  toMarkup (Image (LinkT {..})) = 
    B.div B.! A.class_ "idoc-image-contents" $
          mLabel $ B.img B.! A.class_ "idoc-image-block-image"
                         B.! A.src (toValue linkLocation)
    where
      mLabel = ClassyPrelude.id
--      mLabel = maybe ClassyPrelude.id (\(LinkText x) -> (B.! A.alt (textValue x))) linkText


instance ToMarkup Video where
  toMarkup (Video (LinkT {..})) =
    B.div B.! A.class_ "idoc-video-contents" $
          mLabel $ B.video B.! A.class_ "idoc-video-block-video" 
                           B.! A.controls "true"
                           B.! A.src (toValue linkLocation) $
                           text ""
    where
      mLabel = ClassyPrelude.id
--      mLabel = maybe ClassyPrelude.id (\(LinkText x) -> (B.! A.alt (textValue x))) linkText

instance ToMarkup YouTube where
  toMarkup (YouTube (LinkT {..})) = 
    B.div B.! A.class_ "idoc-youtube-contents" $
          mLabel $ video B.! A.class_ "idoc-youtube-block-youtube"
                         B.! A.src (toValue linkLocation) $ 
                         text ""
    where
      mLabel = ClassyPrelude.id
--      mLabel = maybe ClassyPrelude.id (\(LinkText x) -> (B.! A.alt (textValue x))) linkText

instance ToMarkup Aside where
  toMarkup (Aside mpb x) = 
    B.div B.! A.class_ "idoc-aside-contents" $
          B.aside B.! A.class_ "idoc-aside" $
                  (toMarkup pb) B.! A.class_ "idoc-aside-prerex" ++
                  (B.section B.! A.class_ "idoc-aside-section" $ 
                             toMarkup x)
    where
      pb = maybe "" toMarkup mpb

instance ToMarkup Admonition where
  toMarkup (Admonition x) =
    B.div B.! A.class_ "idoc-admonition-contents" $
          B.aside B.! A.class_ "idoc-admonition" $
                  toMarkup x

instance ToMarkup Sidebar where
  toMarkup (Sidebar x) =
    B.div B.! A.class_ "idoc-sidebar-contents" $
          B.aside B.! A.class_ "idoc-sidebar" $
                  toMarkup x

instance ToMarkup Example where
  toMarkup (Example x) =
    B.div B.! A.class_ "idoc-example-contents" $
          toMarkup x

instance ToMarkup Exercise where
  toMarkup (Exercise x) =
    B.div B.! A.class_ "idoc-exercise-contents" $
          toMarkup x

instance ToMarkup Bibliography where
  toMarkup (Bibliography xs) =
    B.div B.! A.class_ "idoc-bibliography-contents" $
          B.section B.! A.class_ "idoc-bibliography" $
                    B.ol B.! A.class_ "idoc-bibliography-list" $
                         concatMap toMarkup xs

instance (TypedBlock bType, ToMarkup bType) =>
  ToMarkup (BlockT bType) where
  toMarkup (BlockT {..}) =
    mIDV blockID $ 
    B.div B.! A.class_ ("idoc-block " ++ "idoc-" ++ bt ++ "-block") $
    mBTitle ++
    toMarkup blockContents
    where
      bt = case (bType :: BlockType bType) of (BlockType bt) -> toValue bt
      mBTitle = maybe "" toMarkup blockTitle

instance ToMarkup Block where
  toMarkup (BMathBlock x) = toMarkup x
  toMarkup (BEqnArrayBlock x) = toMarkup x
  toMarkup (BTheoremBlock x) = toMarkup x
  toMarkup (BProofBlock x) = toMarkup x
  toMarkup (BPrerexBlock x) = toMarkup x
  toMarkup (BQuoteBlock x) = quoteBlockMarkup x
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

instance ToMarkup DocHeading where
  toMarkup (DocHeading x) =
    B.h1 B.! A.class_ "idoc-title" $
    toMarkup x

instance ToMarkup SectionHeading where
  toMarkup (SectionHeading x) =
    B.h2 B.! A.class_ "idoc-section-title" $
         toMarkup x

instance ToMarkup SubsectionHeading where
  toMarkup (SubsectionHeading x) =
    B.h3 B.! A.class_ "idoc-subsection-title" $
         toMarkup x

instance LineLike a => ToMarkup (ComplexContent a) where
  toMarkup (CCBlock x) = toMarkup x
  toMarkup (CCList x) = toMarkup x
  toMarkup (CCParagraph x) = toMarkup x

instance LineLike a => ToMarkup (Section a) where
  toMarkup (Section {..}) =
    mIDV sectionID $
    B.section B.! A.class_ "idoc-section" $
              toMarkup sectionTitle ++
              as ++
              (concatMap toMarkup $ snd sectionContents)
    where
      as = maybe "" toMarkup $ fst sectionContents

instance LineLike a => ToMarkup (Subsection a) where
  toMarkup (Subsection {..}) =
    mIDV subsectionID $
    B.section B.! A.class_ "idoc-subsection" $
              toMarkup subsectionTitle ++
              toMarkup subsectionContents

instance LineLike a => ToMarkup (AnonymousSection a) where
  toMarkup (AnonymousSection xs) =
    concatMap toMarkup xs

instance LineLike a => ToMarkup (Content a) where
  toMarkup (TLCSubsection x) = toMarkup x
  toMarkup (TLCSection x) = toMarkup x
  toMarkup (TLCAnonymousSection x) = toMarkup x

instance LineLike a => ToMarkup (Doc a) where
  toMarkup (Doc {..}) = 
    B.article B.! A.class_ "idoc-doc container" $
              toMarkup docTitle ++
              dp ++
              concatMap toMarkup docContents
    where
      dp = maybe "" toMarkup docPrerex

renderPretty :: LineLike a => (Doc a) -> String
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
  
