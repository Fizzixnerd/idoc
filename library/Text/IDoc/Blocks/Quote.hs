module Text.IDoc.Blocks.Quote where

import Text.IDoc.Syntax
import Text.IDoc.Parse

import Text.Blaze.Html5

import Data.Data

import Control.Lens

import ClassyPrelude

data Quote = Quote { _quoteContents :: Vector SimpleCore }
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

instance ToMarkup Quote where
  toMarkup (Quote q_) = vectorBlockToMarkup "idocQuote" id q_

quoteP :: IDocParser Quote
quoteP = Quote <$> simpleCoreBlockP

makeLenses ''Quote
