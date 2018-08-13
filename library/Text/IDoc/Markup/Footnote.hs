module Text.IDoc.Markup.Footnote where

import Data.Data

import Text.IDoc.Syntax
import Text.IDoc.Parse

import Text.Blaze.Html.Renderer.Text
import Text.Blaze.Html5 as B
import Text.Blaze.Html5.Attributes as A hiding (span)

import Text.LaTeX as L

import Control.Lens

import ClassyPrelude hiding (span, id)

data Footnote m = Footnote { _footnoteContents :: Vector (SimpleCore m)
                           , _footnoteSetID    :: SetID m }
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

makeLenses ''Footnote

-- | FIXME: Make this properly done.  It doesn't used the `SetID' it's
-- given, etc.
instance MarkupMarkup m => MarkupMarkup (Footnote m) where
  markupMarkup _ _ fn = span ! class_ "d-inline-block"
                             ! id (toValue $ fn^.footnoteSetID)
                             ! dataAttribute "toggle" "tooltip"
                             ! A.title (toValue $ renderHtml $ concatMap toMarkup $ fn^.footnoteContents) $
                             a $ sup (text "[note]")

instance Markupy m => Markupy (Footnote m) where
  markupy _ msid fn = mLabel msid $ footnote $ (texy $ fn^.footnoteSetID) ++ (concatMap texy $ fn^.footnoteContents)

footnoteP :: IDocParser m b (Footnote m)
footnoteP = uncurry Footnote <$> markupContentsWithSetIDP
