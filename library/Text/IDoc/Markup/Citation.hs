module Text.IDoc.Markup.Citation where

import Data.Data

import Text.IDoc.Syntax
import Text.IDoc.Parse

import Text.Blaze.Html5
import Text.Blaze.Html5.Attributes hiding (span)

import Text.LaTeX as L

import Control.Lens

import ClassyPrelude hiding (span, id)

data Citation = Citation { _citationContents :: IDHash }
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

makeLenses ''Citation

instance MarkupMarkup Citation where
  markupMarkup _ _ c = a ! class_ "idocCitation"
                         ! href (toValue $ c^.citationContents) $ ""

instance Markupy Citation where
  markupy _ msid c = mLabel msid $ L.cite $ texy $ c^.citationContents

citationP :: IDocParser m b Citation
citationP = Citation <$> markupIDHashP
