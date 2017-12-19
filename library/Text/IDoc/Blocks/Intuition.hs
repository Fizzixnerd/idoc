module Text.IDoc.Blocks.Intuition where

import Text.IDoc.Syntax
import Text.IDoc.Parse
import Text.IDoc.Render.Html5.Card
import Text.IDoc.Render.Html5.Icons
import Text.IDoc.Render.Tex

import Text.Blaze.Html5

import Data.Data

import Control.Lens

import ClassyPrelude

data Intuition a = Intuition { _intuitionContents :: Vector (Core a) }
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

instance BlockMarkup a => ToMarkup (Intuition a) where
  toMarkup (Intuition i_) = vectorBlockToMarkup "idocIntuition" id i_

instance BlockMarkup a => BlockMarkup (Intuition a) where
  blockMarkup _ t s i_ = card primaryCardOptions (mTitle "Intuition" t) s (icon "fa-puzzle-piece") Nothing (toMarkup i_)

intuitionP :: BlockParser a -> IDocParser (Intuition a)
intuitionP b_ = Intuition <$> coreBlockP b_

instance Blocky a => Blocky (Intuition a) where
  block _ mt msid (Intuition i_) = intuitionBlock (mLabel msid title_) (vectorTexy i_)
    where title_ = mTitleT mt "Intuition"

makeLenses ''Intuition
