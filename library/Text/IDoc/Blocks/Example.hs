module Text.IDoc.Blocks.Example where

import Text.IDoc.Syntax
import Text.IDoc.Parse
import Text.IDoc.Render.Html5.Card
import Text.IDoc.Render.Html5.Icons

import Data.Data

import Control.Lens

import ClassyPrelude

data Example a = Example { _exampleQuestion :: Vector (Core a)
                         , _exampleSolution :: Vector (Core a) }
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

instance BlockMarkup a => BlockMarkup (Example a) where
  blockMarkup _ t s (Example ques sol) = card 
                                         defaultCardOptions 
                                         (mTitle "Example" t) 
                                         s 
                                         (icon "fa-pencil")
                                         (Just $ vectorBlockToMarkup "idocExampleSolution" id sol)
                                         (vectorBlockToMarkup "idocExampleQuestion" id ques)

exampleP :: BlockParser a -> IDocParser (Example a)
exampleP b_ = do
  (q_, s) <- doubleCoreBlockP b_
  return $ Example q_ s

makeLenses ''Example

