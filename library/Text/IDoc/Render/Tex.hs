{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
-- | Tex.hs
-- Author: Matt Walker
-- License: https://opensource.org/licenses/BSD-2-Clause
-- Created: Mar 29, 2017
-- Summary: Render an Doc to LaTeX.

{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- {-# LANGUAGE TemplateHaskell #-}
-- {-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}

module Text.IDoc.Render.Tex where

import ClassyPrelude
import Text.Megaparsec
import Text.IDoc.Parse
import System.IO

newtype Tex = Tex Text deriving (Eq, Show, IsString)

tween :: Tex -> Tex -> Tex -> Tex
tween beg end x = beg <> x <> end 

begEnd :: Text -> Tex -> Tex
begEnd env x = tween 
               ((macro1 "begin" $ render env) <> renderT "\n")
               (renderT "\n" <> (macro1 "end" $ render env))
               x

renderT :: Text -> Tex
renderT = render 

unTex :: Tex -> Text
unTex (Tex x) = x

utRender :: Render a => a -> Text
utRender = unTex . render

esc :: Tex -> Tex
esc (Tex xs) = Tex $ 
               replaceSeq "#" "\\#" $  
               replaceSeq "$" "\\$" $  
               replaceSeq "_" "\\_" $
               replaceSeq "^" "\\^" $
               replaceSeq "~" "\\~" $
               replaceSeq "}" "\\}" $
               replaceSeq "{" "\\{" $ 
               replaceSeq "%" "\\%" $
               replaceSeq "\\" "\\textbackslash" $
               xs

macro0 :: Text -> Tex
macro0 com = Tex $ "\\" <> com
  
macro1 :: Text -> Tex -> Tex
macro1 com x = Tex $ "\\" <> com <> "{" <> unTex x <> "}"

macro1' :: Text -> Tex -> Tex
macro1' com x = Tex $ "\\" <> com <> "[" <> unTex x <> "]"

macro2 :: Text -> Tex -> Tex -> Tex
macro2 com x y = Tex $ "\\" <> com <> "{" <> unTex x <> "}{" <> unTex y <> "}"

label :: IDHash -> Tex
label (IDHash labelName) = macro1 "label" $ render labelName

href :: Tex -> Tex -> Tex
href = macro2 "href"

instance Monoid Tex where
  mappend = (<>)
  mempty = renderT ""

instance Semigroup Tex where
  (Tex x) <> (Tex y) = Tex (x <> y)

class Render a where
  render :: a -> Tex

instance Render Text where
  render t = esc $ Tex t

instance Render IDPathComponent where
  render (IDPathComponent x) = esc $ Tex x

instance Render IDPath where
  render (IDPath xs) = Tex $ concat $ intersperse "/" $ utRender <$> xs

instance Render IDHash where
  render (IDHash x) = render x

instance Render ID where
  render (ID { .. }) = Tex $ utRender idPath <> "#" <> utRender idHash

instance Render SetID where
  render (SetID (IDHash x)) = macro1 "label" $ render $ IDHash $ drop 1 x

instance Render AttrName where
  render (AttrName x) = esc $ Tex x

instance Render AttrList where
  render (AttrList xs) = Tex $ "[" <> (concat $ intersperse "," $ utRender <$> xs) <> "]"

instance Render BlockTitle where
  render (BlockTitle x) = macro1 "subsubsection" $ render x

instance Render MathText where
  render (MathText x) = Tex x -- note: DO NOT ESCAPE

instance Render InlineMath where
  render (InlineMath x) = Tex $ " $" <> utRender x <> "$"

instance Render CommentLine where
  render (CommentLine x) = Tex "% " <> render x

instance Render Protocol where
  render (Protocol x) = render x

instance Render URI where
  render (URI x) = render x

instance Render CommonLink where
  render (CommonLink ("http", u)) = Tex $ "http://" <> utRender u
  render (CommonLink ("https", u)) = Tex $ "https://" <> utRender u
  render (CommonLink (p, _)) = error $ unpack $ "cannot render CommonLink of type: " <> utRender p

instance Render Back where
  render (Back x) = render x

instance Render Out where
  render (Out x) = Tex $ "https://learn.independentlearning.science/" <> utRender x

instance Render Internal where
  render (Internal x) = render x

instance Render LinkText where
  render (LinkText x) = render x

instance Render protocol => Render (LinkT protocol) where
  render (LinkT {..}) = href (render linkLocation) lt
    where
      lt = maybe (render linkLocation) render linkText

instance Render Link where
  render (LBLink x) = render x
  render (LOLink x) = render x
  render (LILink x) = render x

instance Render (QTextT Bold) where
  render (QTextT {..}) = macro1 "textbf" (render qtextText)

instance Render (QTextT Italic) where
  render (QTextT {..}) = macro1 "emph" (render qtextText)

instance Render (QTextT Monospace) where
  render (QTextT {..}) = macro1 "texttt" (render qtextText)

instance Render (QTextT Superscript) where
  render (QTextT {..}) = macro1 "textsuperscript" (render qtextText)

instance Render (QTextT Subscript) where
  render (QTextT {..}) = macro1 "textsubscript" (render qtextText)

instance Render QText where
  render (QTBoldText x) = render x
  render (QTItalicText x) = render x
  render (QTMonospaceText x) = render x
  render (QTSuperscriptText x) = render x
  render (QTSubscriptText x) = render x

instance Render Footnote where
  render (Footnote {..}) = macro1 "footnote" $ mLabel <> (render $ footnoteContent)
    where
      mLabel = maybe (renderT "") (macro1 "label") (render <$> footnoteID)

instance Render FootnoteRef where
  render (FootnoteRef {..}) = macro1' "footnotemark" $ macro1 "ref" (render footnoteRefID)

instance Render PlainText where
  render (PlainText x) = render x

instance Render SimpleContent where
  render (SCInlineMath x) = render x
  render (SCLink x) = render x
  render (SCFootnote x) = render x
  render (SCFootnoteRef x) = render x
  render (SCQText x) = render x

instance Render ParagraphContent where
  render (PCSimpleContent x) = render x
  render (PCPlainText x) = render x

instance Render Paragraph where
  render (Paragraph (xs, _)) = concat $ render <$> xs

instance Render labelType => 
  Render (ListItem labelType) where
  render (ListItem {..}) = 
    let lbl = render listItemLabel
        lnk = maybe (renderT "") render listItemID
        cnts = render listItemContents
    in
      lbl <> lnk <> cnts

instance Render Unordered where 
  render _ = macro0 "item" <> renderT " "

instance Render Ordered where
  render _ = macro0 "item" <> renderT " "

instance Render Labelled where
  render (Labelled x) = (macro1 "item" $ render x) <> renderT " "

instance Render labelType =>
  Render (ListT labelType) where
  render (ListT {..}) = 
    let itms = concat $ intersperse (renderT "\n") $ render <$> listItems
        lid  = maybe (renderT "") render listID
    in
      itms <> lid

instance Render List where
  render (LUList l) = begEnd "itemize" $ render l
  render (LOList l) = begEnd "enumerate" $ render l
  render (LLList l) = begEnd "description" $ render l

instance Render ImageLink where render (ImageLink x) = macro1 "href" $ render x
instance Render VideoLink where render (VideoLink x) = macro1 "href" $ render x
instance Render YouTubeLink where render (YouTubeLink x) = macro1 "href" $ render x
instance Render BibliographyContent where render (BibliographyContent x) = render x

renderBid :: (Maybe SetID) -> Tex
renderBid bid = maybe mempty (\x -> render x <> renderT " ") bid

class RenderLabelled a l | a -> l where
  renderL :: l -> a -> Tex

instance RenderLabelled Math (Maybe SetID) where
  renderL bid (Math x) = begEnd "displaymath" $ renderBid bid <> Tex x

instance RenderLabelled EqnArray (Maybe SetID) where
  renderL bid (EqnArray xs) = begEnd "eqnarray" $ renderBid bid <> (concat $ intersperse (renderT "\\\n") $ renderL Nothing <$> xs)

instance RenderLabelled Theorem (Maybe SetID) where
  renderL bid (Theorem x) = begEnd "Theorem" $ renderBid bid <> renderManyCC x

instance RenderLabelled Proof (Maybe SetID) where
  renderL bid (Proof x) = begEnd "Proof" $ renderBid bid <> renderManyCC x

instance RenderLabelled Quote (Maybe SetID) where
  renderL bid (Quote x) = begEnd "quote" $ renderBid bid <> render x

instance RenderLabelled Code (Maybe SetID) where
  renderL bid (Code x) = begEnd "verbatim" $ renderBid bid <> render x

-- | FIXME
instance RenderLabelled Image (Maybe SetID) where
  renderL bid (Image x) = renderBid bid <> render x

-- | FIXME
instance RenderLabelled Video (Maybe SetID) where
  renderL bid (Video x) = renderBid bid <> render x

-- | FIXME
instance RenderLabelled YouTube (Maybe SetID) where
  renderL bid (YouTube x) = renderBid bid <> render x

renderManyCC :: Vector ComplexContent -> Tex
renderManyCC = concat . (fmap render)

renderLManyCC :: (Maybe SetID) -> Vector ComplexContent -> Tex
renderLManyCC bid = ((renderBid bid) <>) . renderManyCC

-- | FIXME
instance RenderLabelled Aside (Maybe SetID) where renderL bid (Aside (mpb, cnts)) = renderBid bid <> maybe (renderT "") render mpb <> renderManyCC cnts
instance RenderLabelled Admonition (Maybe SetID) where renderL bid (Admonition xs) = renderLManyCC bid xs
instance RenderLabelled Sidebar (Maybe SetID) where renderL bid (Sidebar xs) = renderLManyCC bid xs
instance RenderLabelled Example (Maybe SetID) where renderL bid (Example xs) = renderLManyCC bid xs
instance RenderLabelled Exercise (Maybe SetID) where renderL bid (Exercise xs) = renderLManyCC bid xs
instance RenderLabelled Bibliography (Maybe SetID) where renderL bid (Bibliography xs) = renderBid bid <> (concat $ fmap render xs)
instance RenderLabelled Prerex (Maybe SetID) where renderL _ _ = renderT "\n"

instance RenderLabelled blockType (Maybe SetID) => 
  Render (BlockT blockType) where
  render (BlockT {..}) = 
    let title = maybe (renderT "") (\x -> render x <> renderT "\n") blockTitle
        cnts  = renderL blockID blockContents
    in
      title <> cnts

instance Render Block where
  render (BMathBlock x) = render x
  render (BEqnArrayBlock x) = render x
  render (BTheoremBlock x) = render x
  render (BProofBlock x) = render x
  render (BPrerexBlock x) = render x
  render (BQuoteBlock x) = render x
  render (BCodeBlock x) = render x
  render (BImageBlock x) = render x
  render (BVideoBlock x) = render x
  render (BAdmonitionBlock x) = render x
  render (BAsideBlock x) = render x
  render (BYouTubeBlock x) = render x
  render (BSidebarBlock x) = render x
  render (BExampleBlock x) = render x
  render (BExerciseBlock x) = render x
  render (BBibliographyBlock x) = render x

instance Render Title where render (Title x) = macro1 "title" $ render x
instance Render Section where render (Section x) = macro1 "section" $ render x
instance Render Subsection where render (Subsection x) = macro1 "subsection" $ render x

instance Render htype =>
  Render (Heading htype) where
  render (Heading {..}) = cnts <> renderT "\n" <> sid
    where
      cnts = render hContents
      sid = maybe (renderT "") render hID

instance Render ComplexContent where
  render (CCBlock x) = render x
  render (CCList x) = render x
  render (CCParagraph x) = render x

instance Render TopLevelContent where
  render (TLCSubsectionHeading x) = render x
  render (TLCSectionHeading x)    = render x
  render (TLCCommentLine x)       = render x
  render (TLCComplexContent x)    = render x

instance Render Doc where
  render (Doc {..}) = 
    preamble <> begEnd "document" cnts
    where
      preamble = Tex $ unlines [ "\\documentclass[12pt]{article}"
                               , ""
                               , "\\usepackage{amsmath,amsthm,amssymb}"
                               , "\\usepackage{geometry}"
                               , "\\usepackage{hyperref}"
                               , "\\usepackage{braket}"
                               , ""
                               , "\\newtheorem{Theorem}{Theorem}"
                               , "\\newtheorem{Example}{Example}"
                               , ""
                               , utRender docTitle
                               , ""
                               ]
      cnts = Tex (unlines [ "\\maketitle"
                          , "\\tableofcontents"
                          , ""
                          ])
             <> (concat $ intersperse (Tex "\n\n") $ fmap render $ docContents)

testRenderDoc = (withFile "/home/matt/src/idoc/examples/basic-syntax.idoc" ReadMode 
                 (\h -> do
                     c <- System.IO.hGetContents h
                     System.IO.print $ (fmap render) (parse (parseDoc :: ParsecT Dec String Identity Doc) "../../../../examples/basic-syntax.idoc" c)))
                                                                                              
