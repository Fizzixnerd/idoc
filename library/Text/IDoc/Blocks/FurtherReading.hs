module Text.IDoc.Blocks.FurtherReading where

import Text.IDoc.Syntax
import Text.IDoc.Parse
import Text.IDoc.Render.Html5.Card
import Text.IDoc.Render.Tex

import Text.Blaze.Html5

import Text.LaTeX

import Data.Data

import Control.Lens

import ClassyPrelude

data FurtherReading a = FurtherReading { _furtherReadingContents :: Vector (Core a) }
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

instance BlockMarkup a => ToMarkup (FurtherReading a) where
  toMarkup (FurtherReading f) = vectorBlockToMarkup "idocFurtherReading" id f

-- FIXME: Needs icon
instance BlockMarkup a => BlockMarkup (FurtherReading a) where
  blockMarkup _ t s fr = card defaultCardOptions (mTitle "Further Reading" t) s "" Nothing (toMarkup fr)

instance Blocky a => Blocky (FurtherReading a) where
  block _ mt msid (FurtherReading f) = (subsection $ mLabel msid title_) ++
                                       vectorTexy f
    where
      title_ = mTitleT mt "Further Reading"

furtherReadingP :: BlockParser a -> IDocParser (FurtherReading a)
furtherReadingP b_ = FurtherReading <$> coreBlockP b_

makeLenses ''FurtherReading
