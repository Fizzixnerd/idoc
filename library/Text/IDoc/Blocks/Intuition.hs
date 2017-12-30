module Text.IDoc.Blocks.Intuition where

import Text.IDoc.Syntax
import Text.IDoc.Parse
import Text.IDoc.Render.Html5.Card
import Text.IDoc.Render.Html5.Icons
import Text.IDoc.Render.Tex

import Data.Data

import Control.Lens

import ClassyPrelude

data Intuition m b = Intuition { _intuitionContents :: Vector (Core m b) }
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

instance (MarkupMarkup m, BlockMarkup m (b m)) => BlockMarkup m (Intuition m b) where
  blockMarkup _ t s (Intuition i_) = card
                                     primaryCardOptions
                                     (mTitle "Intuition" t)
                                     s
                                     (icon "fa-puzzle-piece")
                                     Nothing
                                     (vectorBlockToMarkup "idocIntuition" id i_)

intuitionP :: MarkupParser m -> BlockParser m b -> IDocParser (Intuition m b)
intuitionP m b_ = Intuition <$> coreBlockP m b_

instance (Markupy m, Blocky m (b m)) => Blocky m (Intuition m b) where
  blocky _ mt msid (Intuition i_) = intuitionBlock (mLabel msid title_) (vectorTexy i_)
    where title_ = mTitleT mt "Intuition"

makeLenses ''Intuition
