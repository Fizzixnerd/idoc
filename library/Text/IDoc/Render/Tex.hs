-- | Tex.hs
-- Author: Matt Walker
-- License: https://opensource.org/licenses/BSD-2-Clause
-- Created: Mar 29, 2017
-- Summary: Render an Doc to LaTeX.

-- {-# LANGUAGE QuasiQuotes #-}
-- {-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Text.IDoc.Render.Tex where

import ClassyPrelude
import Text.IDoc.Parse

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
  render (SetID x) = macro1 "label" $ render $ x

instance Render BlockTitle where
  render (BlockTitle x) = macro1 "minisec" $ render x

instance Render MathText where
  render (MathText x) = Tex x -- note: DO NOT ESCAPE

instance Render InlineMath where
  render (InlineMath x) = Tex $ "$" <> utRender x <> "$"

instance Render CommentLine where
  render (CommentLine x) = Tex "% " <> render x

instance Render Protocol where
  render (Protocol x) = render x

instance Render URI where
  render (URI x) = render x

instance Render CommonLink where
  render (CommonLink (p, u)) = render p <> render u

instance Render Back where
  render (Back x) = Tex $ "https://learn.independentlearning.science/" <> utRender x

instance Render Out where
  render (Out x) = render x

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
  render (FootnoteRef {..}) = macro1 "footref" $ render footnoteRefID

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
  render (Labelled x) = (macro1' "item" $ render x) <> renderT " "

longestLabel :: ListT Labelled -> Tex
longestLabel (ListT {..}) = 
   (\(ListItem {listItemLabel = Labelled x}) -> renderT x) 
   $ maximumByEx (\(ListItem {listItemLabel = Labelled x}) 
                   (ListItem {listItemLabel = Labelled y}) -> compare (length x) (length y)) listItems


instance Render (ListT Unordered) where
  render (ListT {..}) = 
    let itms = concat $ intersperse (renderT "\n") $ render <$> listItems
        lid  = maybe (renderT "") render listID
    in
      itms <> lid

instance Render (ListT Ordered) where
  render (ListT {..}) = 
    let itms = concat $ intersperse (renderT "\n") $ render <$> listItems
        lid  = maybe (renderT "") render listID
    in
      itms <> lid

instance Render (ListT Labelled) where
  render (l@ListT {..}) = 
    let itms = concat $ intersperse (renderT "\n") $ render <$> listItems
        lid  = maybe (renderT "") render listID
    in
      Tex "{" <> longestLabel l <> Tex "}" <>itms <> lid

instance Render List where
  render (LUList l) = begEnd "itemize" $ render l
  render (LOList l) = begEnd "enumerate" $ render l
  render (LLList l) = begEnd "labeling" $ render l

instance Render ImageLink where render (ImageLink x) = macro1 "href" $ render x
instance Render VideoLink where render (VideoLink x) = macro1 "href" $ render x
instance Render YouTubeLink where render (YouTubeLink x) = macro1 "href" $ render x
instance Render BibliographyItem where render (BibliographyItem x) = render x

renderBid :: (Maybe SetID) -> Tex
renderBid bid = maybe mempty (\x -> render x <> renderT " ") bid

class RenderLabelled a l | a -> l where
  renderL :: l -> a -> Tex

instance RenderLabelled MathContents (Maybe SetID) where
  renderL bid (MathContents x) = begEnd "displaymath" $ renderBid bid <> Tex x
instance RenderLabelled Math (Maybe SetID) where
  renderL bid (Math x _) = renderL bid x

instance RenderLabelled EqnArrayContents (Maybe SetID) where
  renderL bid (EqnArrayContents xs) = begEnd "eqnarray" $ renderBid bid <> (concat $ intersperse (renderT "\\\n") $ render <$> xs)
instance RenderLabelled EqnArray (Maybe SetID) where
  renderL bid (EqnArray x _) = renderL bid x

instance RenderLabelled TheoremContents (Maybe SetID) where
  renderL bid (TheoremContents x) = begEnd "Theorem" $ renderBid bid <> renderManyCC x
instance RenderLabelled Theorem (Maybe SetID) where 
  renderL bid (Theorem x _) = renderL bid x

instance RenderLabelled ProofContents (Maybe SetID) where
  renderL bid (ProofContents x) = begEnd "Proof" $ renderBid bid <> renderManyCC x
instance RenderLabelled Proof (Maybe SetID) where
  renderL bid (Proof x _) = renderL bid x

instance RenderLabelled QuoteContents (Maybe SetID) where
  renderL bid (QuoteContents x) = begEnd "quote" $ renderBid bid <> render x
instance RenderLabelled Quote (Maybe SetID) where
  renderL bid (Quote x _) = renderL bid x

instance RenderLabelled CodeContents (Maybe SetID) where
  renderL bid (CodeContents x) = begEnd "verbatim" $ renderBid bid <> render x
instance RenderLabelled Code (Maybe SetID) where
  renderL bid (Code x _) = renderL bid x

-- | FIXME
instance RenderLabelled ImageContents (Maybe SetID) where
  renderL bid (ImageContents x) = renderBid bid <> render x
instance RenderLabelled Image (Maybe SetID) where
  renderL bid (Image x _) = renderL bid x

-- | FIXME
instance RenderLabelled VideoContents (Maybe SetID) where
  renderL bid (VideoContents x) = renderBid bid <> render x
instance RenderLabelled Video (Maybe SetID) where
  renderL bid (Video x _) = renderL bid x

-- | FIXME
instance RenderLabelled YouTubeContents (Maybe SetID) where
  renderL bid (YouTubeContents x) = renderBid bid <> render x
instance RenderLabelled YouTube (Maybe SetID) where
  renderL bid (YouTube x _) = renderL bid x

renderManyCC :: Vector ComplexContent -> Tex
renderManyCC = concat . (fmap render)

renderLManyCC :: (Maybe SetID) -> Vector ComplexContent -> Tex
renderLManyCC bid = ((renderBid bid) <>) . renderManyCC

-- | FIXME
instance RenderLabelled AsideContents (Maybe SetID) where 
  renderL bid (AsideContents (mpb, cnts)) = renderBid bid <> 
                                            maybe (renderT "") render mpb 
                                            <> renderManyCC cnts
instance RenderLabelled Aside (Maybe SetID) where
  renderL bid (Aside x _) = renderL bid x

instance RenderLabelled Admonition (Maybe SetID) where 
  renderL bid (Admonition (AdmonitionContents xs) _) = renderLManyCC bid xs

instance RenderLabelled Sidebar (Maybe SetID) where 
  renderL bid (Sidebar (SidebarContents xs) _) = macro1 "Margin" $ renderLManyCC bid xs

instance RenderLabelled Example (Maybe SetID) where 
  renderL bid (Example (ExampleContents xs) _) = renderLManyCC bid xs

instance RenderLabelled Exercise (Maybe SetID) where 
  renderL bid (Exercise (ExerciseContents xs) _) = renderLManyCC bid xs

instance RenderLabelled Bibliography (Maybe SetID) where 
  renderL bid (Bibliography (BibliographyContents xs) _) = renderBid bid <> (concat $ fmap render xs)

instance RenderLabelled Prerex (Maybe SetID) where 
  renderL _ _ = renderT "\n"

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
      preamble = Tex $ unlines [ "\\documentclass[mpinclude=true]{scrartcl}"
                               , ""
                               , "\\usepackage{amsmath,amsthm,amssymb}"
                               , ""
                               , "\\usepackage[utf8]{inputenc}"
                               , "\\usepackage{scrlayer-notecolumn}"
                               , "\\usepackage{setspace}"
                               , "\\usepackage{hyperref}"
                               , "\\usepackage{braket}"
                               , ""
                               , "\\newtheorem{Theorem}{Theorem}"
                               , ""
                               , "\\setlength{\\marginparwidth}{2.67\\marginparwidth}"
                               , "\\KOMAoption{headings}{normal}"
                               , "\\KOMAoption{captions}{outerbeside}"
                               , "\\setkomafont{labelinglabel}{\\sffamily\\bfseries}"
                               , "\\newcommand{\\Margin}[1]{\\makenote{#1}}"
                               , ""
                               , utRender docTitle
                               , "\\subtitle{An \\href{https://www.independentlearning.science}{ILS} Document}"
                               , ""
                               ]
      cnts = Tex (unlines [ "\\maketitle"
                          , "\\tableofcontents"
                          , "\\pagestyle{headings}"
                          , ""
                          ])
             <> (concat $ intersperse (Tex "\n\n") $ fmap render $ docContents)
