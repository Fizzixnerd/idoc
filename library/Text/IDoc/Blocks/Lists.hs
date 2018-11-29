{-# LANGUAGE DeriveFunctor #-}

module Text.IDoc.Blocks.Lists where

import ClassyPrelude
import Control.Lens hiding (List)
import Data.Data
import qualified Data.Vector as V
import Text.Printf

import Text.Blaze.Html5
import Text.Blaze.Html5.Attributes
import Text.LaTeX as L

import Text.IDoc.Render.Tex
import Text.IDoc.Syntax

-- FIXME: Add a number to each constructor for nested lists.
-- | The type of a `List', whether "ordered", "unordered", or "labelled".
data ListType = Unordered
              | Ordered
              | Labelled
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

-- FIXME: Move `_liType' from `ListItem' to `List'
-- | A List of things, either `Ordered', `Unordered', or `Labelled' (see
-- `ListType'). Represents things like lists of bullet points, numbered lists,
-- or lists of definitions, etc.
data List m = List { _listContents :: Vector (ListItem m) }
  deriving (Eq, Ord, Show, Data, Typeable, Generic, Functor)

-- | A label for a `ListItem' in `Labelled' `List's.
newtype ListLabel m = ListLabel { unListLabel :: Vector (SimpleCore m) }
  deriving (Eq, Ord, Show, Data, Typeable, Generic, Functor)

-- | A single item in a `List'. Can be `Link'ed to via its `SetID'. Currently
-- only contains `SimpleCore' contents. (No nested lists, I'm afraid. This
-- should change soon.)
data ListItem m = ListItem { _liAttrs :: AttrMap
                           , _liLabel :: Maybe (ListLabel m)
                           , _liContents :: Vector (SimpleCore m)
                           , _liSetID :: Maybe (SetID m)
                           , _liType :: ListType
                           }
  deriving (Eq, Ord, Show, Data, Typeable, Generic, Functor)

makeLenses ''List
makeLenses ''ListItem

instance MarkupMarkup m => ToMarkup (ListItem m) where
  toMarkup li_ = correctListItemHolder (li_^.liType) (li_^.liLabel) $
                 concatMap toMarkup $ li_^.liContents
    where
      correctListItemHolder Labelled (Just l) =
        (\x -> (dt ! class_ "idocLabel" $ toMarkup l) ++
               (dd ! class_ "idocLabelledItem" $ x))
      correctListItemHolder Ordered Nothing = li ! class_ "idocOrderedItem"
      correctListItemHolder Unordered Nothing = li ! class_ "idocUnorderedItem"
      correctListItemHolder _ _ = fail $ printf "Failed to match pattern in ToMarkup (ListItem m)."

instance MarkupMarkup m => ToMarkup (ListLabel m) where
  toMarkup (ListLabel ll_) = concatMap toMarkup ll_

instance MarkupMarkup m => ToMarkup (List m) where
  toMarkup (List l) = correctListHolder ((V.head l)^.liType) $
                      concatMap toMarkup l
    where
      correctListHolder Unordered = ul ! class_ "idocUnorderedList"
      correctListHolder Ordered = ol ! class_ "idocOrderedList"
      correctListHolder Labelled = dl ! class_ "idocLabelledList"

instance Markupy m => Texy (List m) where
  texy (List li_) = enumerate $
                    concatMap (\li'_ ->
                                  mLabel (li'_^.liSetID) $
                                  L.item (textbf <$> texy <$> (li'_^.liLabel)) ++ (vectorTexy $ li'_^.liContents)) li_

instance Markupy m => Texy (ListLabel m) where
  texy (ListLabel ll_) = vectorTexy ll_

instance CheckLinks m b m => CheckLinks m b (List m) where
  checkLinks constraints container (List list_) =
    concatMap (\listItem ->
                  (maybe mempty (checkLinks constraints container) (listItem^.liLabel))
                  ++ (concatMap (checkLinks constraints container) (listItem^.liContents))) list_

instance CheckLinks m b m => CheckLinks m b (ListLabel m) where
  checkLinks constraints container (ListLabel llc) =
    concatMap (checkLinks constraints container) llc
