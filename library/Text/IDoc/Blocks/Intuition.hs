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

instance (CheckLinks m b m, CheckLinks m b (b m)) => CheckLinks m b (Intuition m b) where
  checkLinks constraints container (Intuition ic) =
    concatMap (checkLinks constraints container) ic

intuitionP :: IDocParser m b (Intuition m b)
intuitionP = Intuition <$> coreBlockP

instance (Markupy m, Blocky m (b m)) => Blocky m (Intuition m b) where
  blocky _ mt msid (Intuition i_) = intuitionBlock (mLabel msid title_) (vectorTexy i_)
    where title_ = mTitleT mt "Intuition"

makeLenses ''Intuition
