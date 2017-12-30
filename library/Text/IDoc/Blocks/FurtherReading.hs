module Text.IDoc.Blocks.FurtherReading where

import Text.IDoc.Syntax
import Text.IDoc.Parse
import Text.IDoc.Render.Html5.Card
import Text.IDoc.Render.Tex

import Text.LaTeX

import Data.Data

import Control.Lens

import ClassyPrelude

data FurtherReading m b = FurtherReading { _furtherReadingContents :: Vector (Core m b) }
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

-- FIXME: Needs icon
instance (MarkupMarkup m, BlockMarkup m (b m)) => BlockMarkup m (FurtherReading m b) where
  blockMarkup _ t s (FurtherReading fr) = card
                                          defaultCardOptions
                                          (mTitle "Further Reading" t)
                                          s
                                          ""
                                          Nothing
                                          (vectorBlockToMarkup "idocFurtherReading" id fr)

instance (Markupy m, Blocky m (b m)) => Blocky m (FurtherReading m b) where
  blocky _ mt msid (FurtherReading f) = (subsection $ mLabel msid title_) ++
                                       vectorTexy f
    where
      title_ = mTitleT mt "Further Reading"

furtherReadingP :: MarkupParser m -> BlockParser m b -> IDocParser (FurtherReading m b)
furtherReadingP m b_ = FurtherReading <$> coreBlockP m b_

makeLenses ''FurtherReading
