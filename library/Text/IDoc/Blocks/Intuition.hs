module Text.IDoc.Blocks.Intuition where

import Text.IDoc.Syntax

import Data.Data

import Control.Lens

import ClassyPrelude

data Intuition a = Intuition { _intuitionContents :: Vector (Core a) }
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

makeLenses ''Intuition
