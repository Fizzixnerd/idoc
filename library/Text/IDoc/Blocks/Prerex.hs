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

import qualified Data.Vector as V

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

data PrerexItem m = PrerexItem
  { _prerexItemPath :: Link m
  , _prerexItemDescription :: Vector (SimpleCore m)
  } deriving (Eq, Ord, Show, Data, Typeable, Generic)

prerexItemP :: IDocParser m b (PrerexItem m)
prerexItemP = do
  r <- view Text.IDoc.Parse.rel
  path <- idP
  desc <- markupContentsP
  return $ PrerexItem { _prerexItemPath = Link { _linkText = Nothing
                                               , _linkAttrs = AttrMap mempty
                                               , _linkLocation = path
                                               , _linkType = Back r
                                               }
                      , _prerexItemDescription = desc
                      }

toConstraints :: Prerex m -> LinkConstraints
toConstraints prerex =
  let items = _prerexContents prerex
  in
    LinkConstraints $ setFromList $ toList $ (\(PrerexItem
       { _prerexItemPath = Link
                           { _linkLocation = ID
                                             { _idBase = base_ }
                           }
       }) -> LinkConstraint base_) <$> items

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

instance CheckLinks m b m => CheckLinks m b (Prerex m) where
  checkLinks constraints container (Prerex pis) =
    concatMap (checkLinks constraints container) pis

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
                     (concatMap toMarkup $ p_^.prerexItemDescription)
    where
      itemPath = toMarkup $ (concatMap _unIDBase $ IDBase "/" `V.cons` (intersperse (IDBase "/") (p_^.prerexItemPath.linkLocation.idBase)))


instance Markupy m => Texy (PrerexItem m) where
  texy p_ = (H.href [] "" $ texy $ fromBack $ p_^.prerexItemPath.linkLocation) ++
            ": " ++
            newline ++
            (concatMap texy $ p_^.prerexItemDescription) ++
            newline
    where fromBack id_ = "/" ++ (concatMap _unIDBase $ intersperse (IDBase "/") (id_^.idBase))

instance CheckLinks m b m => CheckLinks m b (PrerexItem m) where
  checkLinks constraints container (PrerexItem _ pidesc) =
    concatMap (checkLinks constraints container) pidesc
