module Text.IDoc.Blocks.Recall where

import Text.IDoc.Syntax
import Text.IDoc.Parse

import Text.Blaze.Html5

import Data.Data

import Control.Lens

import ClassyPrelude

data Recall a = Recall { _recallLinks :: Vector Link
                       , _recallCore  :: Vector (Core a) }
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

instance BlockMarkup a => ToMarkup (Recall a) where
  toMarkup (Recall _ r) = vectorBlockToMarkup "idocRecall" id r

recallP :: BlockParser a -> IDocParser (Recall a)
recallP b_ = do
  (l, c) <- vectorLinkCoreBlockP b_
  return $ Recall l c

makeLenses ''Recall
