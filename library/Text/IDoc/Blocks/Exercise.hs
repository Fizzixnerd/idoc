module Text.IDoc.Blocks.Exercise where

import Text.IDoc.Syntax
import Text.IDoc.Parse
import Text.IDoc.Render.Html5.Card
import Text.IDoc.Render.Html5.Icons

import Text.Blaze.Html5

import Data.Data

import Control.Lens

import ClassyPrelude

data Exercise a = Exercise { _exerciseContents :: Vector (Core a) }
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

instance BlockMarkup a => ToMarkup (Exercise a) where
  toMarkup (Exercise e) = vectorBlockToMarkup "idocExercise" id e

instance BlockMarkup a => BlockMarkup (Exercise a) where
  blockMarkup _ t s e = card primaryCardOptions (mTitle "Exercise" t) s (icon "fa-pencil") Nothing (toMarkup e)

exerciseP :: BlockParser a -> IDocParser (Exercise a)
exerciseP b_ = Exercise <$> coreBlockP b_

makeLenses ''Exercise
