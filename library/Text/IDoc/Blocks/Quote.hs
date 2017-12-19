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

data Quote = Quote { _quoteContents :: Vector SimpleCore }
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

instance ToMarkup Quote where
  toMarkup (Quote q_) = vectorBlockToMarkup "idocQuote" id q_

instance BlockMarkup Quote where
  blockMarkup (AttrMap a_) t s q_ = card
                                    defaultCardOptions
                                    (mTitle "Quote" t)
                                    s
                                    (icon "fa-quote-left")
                                    ((\(AttrValue v) -> toMarkup v) <$> (a_^.at (AttrName "author").to join))
                                    (toMarkup q_)

instance Blocky Quote where
  block _ _ msid (Quote q_) = mLabel msid $
                              quote $
                              vectorTexy q_

quoteP :: IDocParser Quote
quoteP = Quote <$> simpleCoreBlockP

makeLenses ''Quote
