module Text.IDoc.Blocks.FurtherReading where

import Text.IDoc.Syntax
import Text.IDoc.Parse
import Text.IDoc.Render.Html5.Card

import Text.Blaze.Html5

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

furtherReadingP :: BlockParser a -> IDocParser (FurtherReading a)
furtherReadingP b_ = FurtherReading <$> coreBlockP b_

makeLenses ''FurtherReading
