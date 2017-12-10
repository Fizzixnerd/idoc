module Text.IDoc.Blocks.Prerex where

import Text.IDoc.Syntax

import Data.Data

import Control.Lens

import ClassyPrelude

data Prerex = Prerex { _prerexContents :: Vector PrerexItem }
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

data PrerexItem = PrerexItem { _prerexItemPath :: ID
                             , _prerexItemDescription :: Vector SimpleCore
                             }
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

makeLenses ''Prerex
makeLenses ''PrerexItem
