module Text.IDoc.Blocks.Exercise where

import Text.IDoc.Syntax
import Text.IDoc.Parse
import Text.IDoc.Render.Html5.Card
import Text.IDoc.Render.Html5.Icons
import Text.IDoc.Render.Tex

import Text.LaTeX

import Data.Data

import Control.Lens

import ClassyPrelude

data Exercise m b = Exercise { _exerciseContents :: Vector (Core m b) }
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

instance (MarkupMarkup m, BlockMarkup m (b m)) => BlockMarkup m (Exercise m b) where
  blockMarkup _ t s (Exercise e) = card
                                   primaryCardOptions
                                   (mTitle "Exercise" t)
                                   s
                                   (icon "fa-pencil")
                                   Nothing
                                   (vectorBlockToMarkup "idocExercise" id e)

instance (Markupy m, Blocky m (b m)) => Blocky m (Exercise m b) where
  blocky _ mt msid (Exercise e) = (subsubsection $ mLabel msid title_) ++
                                 vectorTexy e
    where
      title_ = mTitleT mt "Exercise"

instance (CheckLinks m b m, CheckLinks m b (b m)) => CheckLinks m b (Exercise m b) where
  checkLinks constraints container (Exercise e) = concatMap (checkLinks constraints container) e

exerciseP :: IDocParser m b (Exercise m b)
exerciseP = Exercise <$> coreBlockP

makeLenses ''Exercise
