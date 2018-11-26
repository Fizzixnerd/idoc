module Text.IDoc.Markup.FootnoteRef where

import Data.Data

import Text.IDoc.Parse
import Text.IDoc.Syntax

import Text.Blaze.Html5
import Text.Blaze.Html5.Attributes hiding (span)

import Text.LaTeX as L

import Control.Lens

import ClassyPrelude

data FootnoteRef = FootnoteRef { _footnoteRefContents :: IDHash }
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

makeLenses ''FootnoteRef

instance MarkupMarkup FootnoteRef where
  markupMarkup _ _ fnr = a ! class_ "idocFootnoteRef" 
                           ! href (toValue $ fnr^.footnoteRefContents) $ ""

instance Markupy FootnoteRef where
  markupy _ msid fnr = mLabel msid $ footnotemark $ ref $ texy $ fnr^.footnoteRefContents
    where
      footnotemark x = raw "\\footenotemark[" ++ x ++ raw "]"

instance CheckLinks m b FootnoteRef where
  checkLinks _ _ _ = mempty

footnoteRefP :: IDocParser m b FootnoteRef
footnoteRefP = FootnoteRef <$> markupIDHashP
