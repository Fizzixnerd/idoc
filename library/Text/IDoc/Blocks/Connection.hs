module Text.IDoc.Blocks.Connection where

import Text.IDoc.Syntax
import Text.IDoc.Parse
import Text.IDoc.Render.Html5.Card
import Text.IDoc.Render.Html5.Icons
import Text.IDoc.Render.Tex

import Text.Blaze.Html5

import Text.LaTeX

import Data.Data

import Control.Lens

import ClassyPrelude

data Connection a = Connection { _connectionContents :: Vector (Core a) }
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

instance BlockMarkup a => ToMarkup (Connection a) where
  toMarkup (Connection c) = vectorBlockToMarkup "idocConnection" id c

instance BlockMarkup a => BlockMarkup (Connection a) where
  blockMarkup _ title_ sid c = card primaryCardOptions (mTitle "Connection" title_) sid (icon "fa-link") Nothing (toMarkup c)

instance Blocky a => Blocky (Connection a) where
  block _ mt msid (Connection c) = (subsection $ mLabel msid title_) ++
                                   vectorTexy c
    where title_ = mTitleT mt "Connection"

connectionP :: BlockParser a -> IDocParser (Connection a)
connectionP b_ = Connection <$> coreBlockP b_

makeLenses ''Connection
