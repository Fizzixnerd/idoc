module Text.IDoc.Blocks.Exercise where

import Text.IDoc.Syntax

import Data.Data

import Control.Lens

import ClassyPrelude

data Exercise a = Exercise { _exerciseContents :: Vector (Core a) }
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

makeLenses ''Exercise
