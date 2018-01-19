module Text.IDoc.Blocks.Prerex where

import Text.IDoc.Syntax
import Text.IDoc.Parse
import Text.IDoc.Render.Html5.Card
import Text.IDoc.Render.Html5.Icons
import Text.IDoc.Render.Tex

import Text.Blaze.Html5 as B
import Text.Blaze.Html5.Attributes as A

import Text.LaTeX
import Text.LaTeX.Packages.Hyperref as H

import Data.Data

import Control.Lens

import ClassyPrelude hiding (div)

data Prerex m = Prerex { _prerexContents :: Vector (PrerexItem m) }
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

prerexP :: IDocParser m b (Prerex m)
prerexP = Prerex <$> do
  blockStarterP
  someTill blockEnderP $ do
    x <- prerexItemP
    newlineP
    return x

data PrerexItem m = PrerexItem { _prerexItemPath :: Link m }
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

prerexItemP :: IDocParser m b (PrerexItem m)
prerexItemP = do
  r <- view Text.IDoc.Parse.rel
  path <- idP
  desc <- markupContentsP
  return $ PrerexItem { _prerexItemPath = Link { _linkText = Just $ LinkText desc
                                               , _linkAttrs = AttrMap mempty
                                               , _linkLocation = path
                                               , _linkType = Back r
                                               }
                      }

makeLenses ''Prerex
makeLenses ''PrerexItem

-- FIXME: Find an icon for this.
instance MarkupMarkup m => BlockMarkup m (Prerex m) where
  blockMarkup _ t _ p_ = div ! class_ "idocPrerex" $ do
    h3 $ maybe (toMarkup ("Prerex" :: Text)) toMarkup t
    concatMap toMarkup (p_^.prerexContents)

instance Markupy m => Blocky m (Prerex m) where
  blocky _ mt msid (Prerex ps) = subsubsection (mLabel msid title_) ++ vectorTexy ps
    where
      title_ = mTitleT mt "Prerex"

instance MarkupMarkup m => ToMarkup (PrerexItem m) where
  toMarkup p_ = card (defaultCardOptions { cardType = CInfo
                                         , cardDefaultCollapseState = Collapsed
                                         })
                     itemPath
                     (Just $ (p_^.prerexItemPath) { _linkText = Nothing })
                     prerexItemIcon
                     (Just $ a ! class_ "idocPrerexItemLink"
                               ! A.href (p_^.prerexItemPath.to toValue) $
                               "Go to " ++ itemPath)
                     (toMarkup $ maybe (LinkText ClassyPrelude.empty) ClassyPrelude.id (p_^.prerexItemPath.linkText))
    where
      itemPath = toMarkup $ (concatMap _unIDBase $ intersperse (IDBase "/") (p_^.prerexItemPath.linkLocation.idBase))


instance Markupy m => Texy (PrerexItem m) where
  texy p_ = (H.href [] "" $ texy $ fromBack $ p_^.prerexItemPath.linkLocation) ++
            ": " ++
            (p_^.prerexItemPath.to toText.to texy) ++
            newline
    where fromBack id_ = "http://www.independentlearning.science/tiki/" ++ 
                         (concatMap _unIDBase $ intersperse (IDBase "/") (id_^.idBase))

