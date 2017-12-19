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

instance ToMarkup BibTex where
  toMarkup (BibTex b_) = vectorBlockToMarkup "idocBibliography" id b_

-- FIXME: Needs icon
instance BlockMarkup BibTex where
  blockMarkup _ t s bt = card defaultCardOptions (mTitle "Bibliography" t) s "" Nothing (toMarkup bt)

data BibItem = BibItem { _biAuthor :: Text
                       , _biTitle :: Text
                       , _biYear :: Text
                       }
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

instance Blocky BibTex where
  block _ mt msid (BibTex b_) = (subsection $ mLabel msid title_) ++
                                vectorTexy b_
    where
      title_ = mTitleT mt "Bibliography"

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

