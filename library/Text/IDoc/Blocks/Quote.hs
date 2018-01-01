module Text.IDoc.Blocks.Quote where

import Text.IDoc.Syntax
import Text.IDoc.Parse
import Text.IDoc.Render.Html5.Card
import Text.IDoc.Render.Html5.Icons
import Text.IDoc.Render.Tex

import Text.Blaze.Html5

import Text.LaTeX

import Data.Data

import Control.Lens

import ClassyPrelude

data Quote m = Quote { _quoteContents :: Vector (SimpleCore m) }
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

instance MarkupMarkup m => BlockMarkup m (Quote m) where
  blockMarkup (AttrMap a_) t s (Quote q_) = card
                                            defaultCardOptions
                                            (mTitle "Quote" t)
                                            s
                                            (icon "fa-quote-left")
                                            ((\(AttrValue v) -> toMarkup v) <$> (a_^.at (AttrName "author").to join))
                                            (vectorBlockToMarkup "idocQuote" id q_)

instance Markupy m => Blocky m (Quote m) where
  blocky _ _ msid (Quote q_) = mLabel msid $
                               quote $
                               vectorTexy q_

quoteP :: IDocParser m b (Quote m)
quoteP = Quote <$> simpleCoreBlockP

makeLenses ''Quote
