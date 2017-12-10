module Text.IDoc.Blocks.FurtherReading where

import Text.IDoc.Syntax

import Data.Data

import Control.Lens

import ClassyPrelude

data FurtherReading a = FurtherReading { _furtherReadingContents :: Vector (Core a) }
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

makeLenses ''FurtherReading
