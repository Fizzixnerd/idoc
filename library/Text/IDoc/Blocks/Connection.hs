module Text.IDoc.Blocks.Connection where

import Text.IDoc.Syntax

import Data.Data

import Control.Lens

import ClassyPrelude

data Connection a = Connection { _connectionContents :: Vector (Core a) }
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

makeLenses ''Connection
