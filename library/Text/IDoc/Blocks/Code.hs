module Text.IDoc.Blocks.Code where

import Text.IDoc.Syntax

import Data.Data

import Control.Lens

import ClassyPrelude

data Code = Code { _codeContents :: Vector Token }
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

makeLenses ''Code
