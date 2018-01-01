module Text.IDoc.Blocks.Recall where

import Text.IDoc.Syntax
import Text.IDoc.Parse
import Text.IDoc.Render.Html5.Card
import Text.IDoc.Render.Tex

import Text.LaTeX

import Data.Data

import Control.Lens

import ClassyPrelude

data Recall m b = Recall { _recallLinks :: Vector (Link m)
                         , _recallCore  :: Vector (Core m b) }
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

-- FIXME: Requires an icon.
instance (MarkupMarkup m, BlockMarkup m (b m)) => BlockMarkup m (Recall m b) where
  blockMarkup _ t s (Recall _ r) = card
                                   defaultCardOptions
                                   (mTitle "Recall" t)
                                   s
                                   ""
                                   Nothing
                                   (vectorBlockToMarkup "idocRecall" id r)

instance (Markupy m, Blocky m (b m)) => Blocky m (Recall m b) where
  blocky _ mt msid (Recall _ r) = (subsubsection $ mLabel msid title_) ++
                                  vectorTexy r
    where
      title_ = mTitleT mt "Recall"

recallP :: IDocParser m b (Recall m b)
recallP = do
  (l, c) <- vectorLinkCoreBlockP
  return $ Recall l c

makeLenses ''Recall
