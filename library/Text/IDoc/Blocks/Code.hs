module Text.IDoc.Blocks.Code where

import Text.IDoc.Syntax
import Text.IDoc.Parse
import Text.IDoc.Render.Html5.Card
import Text.IDoc.Render.Html5.Icons

import Text.Blaze.Html5

import Data.Data

import Control.Lens

import ClassyPrelude

data Code = Code { _codeContents :: Vector Token }
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

instance ToMarkup Code where
  toMarkup (Code c) = verbatimBlockToMarkup "idocCode" id c

instance BlockMarkup Code where
  blockMarkup _ title_ sid c = card defaultCardOptions (mTitle "Code" title_) sid (icon "fa-code") Nothing (toMarkup c)

codeP :: IDocParser Code
codeP = Code <$> uninterpretedBlockP

makeLenses ''Code
