module Text.IDoc.Blocks.Example where

import Text.IDoc.Syntax

import Data.Data

import Control.Lens

import ClassyPrelude

data Example a = Example { _exampleQuestion :: Vector (Core a)
                         , _exampleSolution :: Vector (Core a) }
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

makeLenses ''Example

