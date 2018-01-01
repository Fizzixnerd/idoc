module Text.IDoc.Blocks.Example where

import Text.IDoc.Syntax
import Text.IDoc.Parse
import Text.IDoc.Render.Html5.Card
import Text.IDoc.Render.Html5.Icons
import Text.IDoc.Render.Tex

import Text.LaTeX

import Data.Data

import Control.Lens

import ClassyPrelude

data Example m b = Example { _exampleQuestion :: Vector (Core m b)
                           , _exampleSolution :: Vector (Core m b) }
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

instance (MarkupMarkup m, BlockMarkup m (b m)) => BlockMarkup m (Example m b) where
  blockMarkup _ t s (Example ques sol) = card
                                         defaultCardOptions
                                         (mTitle "Example" t)
                                         s
                                         (icon "fa-pencil")
                                         (Just $ vectorBlockToMarkup "idocExampleSolution" id sol)
                                         (vectorBlockToMarkup "idocExampleQuestion" id ques)

instance (Markupy m, Blocky m (b m)) => Blocky m (Example m b) where
  blocky _ mt msid (Example ex ans) = (subsubsection $ mLabel msid title_) ++
                                      vectorTexy ex ++
                                      vectorTexy ans
    where
      title_ = mTitleT mt "Example"

exampleP :: IDocParser m b (Example m b)
exampleP = do
  (q_, s) <- doubleCoreBlockP
  return $ Example q_ s

makeLenses ''Example

