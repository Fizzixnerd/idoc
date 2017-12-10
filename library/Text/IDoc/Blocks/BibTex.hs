module Text.IDoc.Blocks.BibTex where

import Data.Data

import Control.Lens

import ClassyPrelude

data BibTex = BibTex { _bibTexContents :: Vector BibItem }
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

data BibItem = BibItem { _biAuthor :: Text
                       , _biTitle :: Text
                       , _biYear :: Text
                       }
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

makeLenses ''BibTex
makeLenses ''BibItem
