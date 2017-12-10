module Text.IDoc.Blocks.Quote where

import Text.IDoc.Syntax

import Data.Data

import Control.Lens

import ClassyPrelude

data Quote = Quote { _quoteContents :: Vector SimpleCore }
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

makeLenses ''Quote
