{-# LANGUAGE RecordWildCards #-}
-- | Tex.hs
-- Author: Matt Walker
-- License: https://opensource.org/licenses/BSD-2-Clause
-- Created: Mar 29, 2017
-- Summary: Render an Doc to LaTeX.

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
import Text.IDoc.Parse

newtype Tex = Tex Text deriving (Eq, Show, IsString)

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
  mempty = render ("" :: Text)

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
  render (SetID x) = Tex $ "[[" <> utRender x <> "]]"

instance Render AttrName where
  render (AttrName x) = esc $ Tex x

instance Render AttrList where
  render (AttrList xs) = Tex $ "[" <> (concat $ intersperse "," $ utRender <$> xs) <> "]"

instance Render BlockTitle where
  render (BlockTitle x) = macro1 "subsubsection" $ render x

instance Render MathText where
  render (MathText x) = Tex x -- note: DO NOT ESCAPE

instance Render InlineMath where
  render (InlineMath x) = Tex $ "$" <> utRender x <> "$"

instance Render CommentLine where
  render (CommentLine x) = Tex "%" <> render x

instance Render Protocol where
  render (Protocol x) = render x

instance Render URI where
  render (URI x) = render x

instance Render CommonLink where
  render (CommonLink ("http", u)) = Tex $ "http://" <> utRender u
  render (CommonLink ("https", u)) = Tex $ "https://" <> utRender u
  render (CommonLink (p, _)) = error $ unpack $ "can't render CommonLink of type: " <> utRender p

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
  render (QTextT {..}) = macro1 "textit" (render qtextText)

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
      mLabel = maybe (render ("" :: Text)) (macro1 "label") (render <$> footnoteID)

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
  render (SCPlainText x) = render x

instance Render Paragraph where
  render (Paragraph xs) = concat $ render <$> xs

