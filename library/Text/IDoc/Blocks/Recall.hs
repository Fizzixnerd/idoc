module Text.IDoc.Blocks.Recall where

import Text.IDoc.Syntax

import Data.Data

import Control.Lens

import ClassyPrelude

data Recall a = Recall { _recallContents :: (Vector Link, Vector (Core a)) }
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

makeLenses ''Recall
