module Text.IDoc.Blocks.Recall where

import Text.IDoc.Syntax
import Text.IDoc.Parse
import Text.IDoc.Render.Html5.Card
import Text.IDoc.Render.Tex

import Text.Blaze.Html5

import Text.LaTeX

import Data.Data

import Control.Lens

import ClassyPrelude

data Recall a = Recall { _recallLinks :: Vector Link
                       , _recallCore  :: Vector (Core a) }
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

instance BlockMarkup a => ToMarkup (Recall a) where
  toMarkup (Recall _ r) = vectorBlockToMarkup "idocRecall" id r

-- FIXME: Requires an icon.
instance BlockMarkup a => BlockMarkup (Recall a) where
  blockMarkup _ t s r = card
                        defaultCardOptions
                        (mTitle "Recall" t)
                        s
                        ""
                        Nothing
                        (toMarkup r)

instance Blocky a => Blocky (Recall a) where
  block _ mt msid (Recall _ r) = (subsubsection $ mLabel msid title_) ++
                                 vectorTexy r
    where
      title_ = mTitleT mt "Recall"

recallP :: BlockParser a -> IDocParser (Recall a)
recallP b_ = do
  (l, c) <- vectorLinkCoreBlockP b_
  return $ Recall l c

makeLenses ''Recall
