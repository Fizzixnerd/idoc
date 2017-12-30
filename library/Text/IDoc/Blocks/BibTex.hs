module Text.IDoc.Blocks.BibTex where

import Text.IDoc.Syntax
import Text.IDoc.Render.Html5.Card
import Text.IDoc.Render.Tex

import Text.Blaze.Html5

import Text.LaTeX

import Text.Printf

import Data.Data

import Control.Lens

import ClassyPrelude

data BibTex = BibTex { _bibTexContents :: Vector BibItem }
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

-- FIXME: Needs icon
instance MarkupMarkup m => BlockMarkup m BibTex where
  blockMarkup _ t s (BibTex b_) = card
                                  defaultCardOptions
                                  (mTitle "Bibliography" t)
                                  s
                                  ""
                                  Nothing
                                  (vectorBlockToMarkup "idocBibliography" id b_)

instance Markupy m => Blocky m BibTex where
  blocky _ mt msid (BibTex b_) = (subsection $ mLabel msid title_) ++
                                 vectorTexy b_
    where
      title_ = mTitleT mt "Bibliography"

data BibItem = BibItem { _biAuthor :: Text
                       , _biTitle :: Text
                       , _biYear :: Text
                       }
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

makeLenses ''BibTex
makeLenses ''BibItem

instance Texy BibItem where
  texy bi = texy (bi^.biTitle) ++
            ": " ++
            texy (bi^.biAuthor) ++
            "; " ++
            texy (bi^.biYear) ++
            "."

instance ToMarkup BibItem where
  toMarkup bi = text $ fromString $ printf "author: %s; title: %s; year: %s" (bi^.biAuthor.to show) (bi^.biTitle.to show) (bi^.biYear.to show)

