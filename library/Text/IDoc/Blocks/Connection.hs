module Text.IDoc.Blocks.Connection where

import Text.IDoc.Syntax
import Text.IDoc.Parse
import Text.IDoc.Render.Html5.Card
import Text.IDoc.Render.Html5.Icons
import Text.IDoc.Render.Tex

import Text.LaTeX

import Data.Data

import Control.Lens

import ClassyPrelude

data Connection m b = Connection { _connectionContents :: Vector (Core m b) }
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

instance (MarkupMarkup m, BlockMarkup m (b m)) => BlockMarkup m (Connection m b) where
  blockMarkup _ title_ sid (Connection c) = card
                                            primaryCardOptions
                                            (mTitle "Connection" title_)
                                            sid
                                            (icon "fa-link")
                                            Nothing
                                            (vectorBlockToMarkup "idocConnection" id c)

instance (Markupy m, Blocky m (b m)) => Blocky m (Connection m b) where
  blocky _ mt msid (Connection c) = (subsection $ mLabel msid title_) ++
                                   vectorTexy c
    where title_ = mTitleT mt "Connection"

connectionP :: MarkupParser m -> BlockParser m b -> IDocParser (Connection m b)
connectionP m b_ = Connection <$> coreBlockP m b_

makeLenses ''Connection
