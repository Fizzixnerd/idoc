{-# LANGUAGE DeriveFunctor #-}

module Text.IDoc.Blocks.Lists where

import ClassyPrelude
import Control.Lens hiding (List)
import Data.Data
import qualified Data.Vector as V
import Text.Printf

import Text.Blaze.Html5 as B
import Text.Blaze.Html5.Attributes as A hiding (id)
import Text.LaTeX as L
import qualified Text.Megaparsec as MP

import Text.IDoc.Render.Tex
import Text.IDoc.Syntax as S
import Text.IDoc.Parse

newtype OrderedItem m b = OrderedItem { _unOrderedItem :: Vector (S.Core m b) }
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

data OrderedList m b = OrderedList
  { _olistContents :: Vector (OrderedItem m b)
  } deriving (Eq, Ord, Show, Data, Typeable, Generic)

newtype UnorderedItem m b = UnorderedItem { _unUnorderedItem :: Vector (S.Core m b) }
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

data UnorderedList m b = UnorderedList
  { _ulistContents :: Vector (UnorderedItem m b)
  } deriving (Eq, Ord, Show, Data, Typeable, Generic)

data DescriptionItem m b = DescriptionItem
  { _descriptionLabel :: Vector (S.SimpleCore m)
  , _descriptionContents :: Vector (S.Core m b)
  } deriving (Eq, Ord, Show, Data, Typeable, Generic)

data DescriptionList m b = DescriptionList
  { _dlistContents :: Vector (DescriptionItem m b)
  } deriving (Eq, Ord, Show, Data, Typeable, Generic)

data ListB m b = OListB (OrderedList m b)
               | UListB (UnorderedList m b)
               | DListB (DescriptionList m b)
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

instance (MarkupMarkup m, BlockMarkup m (b m)) => BlockMarkup m (ListB m b) where
  blockMarkup a_ t s (OListB ol) = blockMarkup a_ t s ol

simpleListItemP :: S.Token -> (Vector (Core m b) -> a) -> IDocParser m b a
simpleListItemP starter constructor = do
  void starterP
  contents_ <- someV $ do
    MP.notFollowedBy $ (some newlineP) >> starterP
    coreP
  return $ constructor contents_
  where
    starterP = tokenP starter

orderedItemP :: IDocParser m b (OrderedItem m b)
orderedItemP = simpleListItemP S.Period OrderedItem

unorderedItemP :: IDocParser m b (UnorderedItem m b)
unorderedItemP = simpleListItemP S.Dash UnorderedItem

descriptionItemP :: IDocParser m b (DescriptionItem m b)
descriptionItemP = do
  void starterP
  label_ <- someV $ do
    MP.notFollowedBy $ starterP >> starterP
    simpleCoreP
  contents_ <- someV $ do
    MP.notFollowedBy $ (some newlineP) >> starterP
    coreP
  return $ DescriptionItem label_ contents_
  where
    starterP = tokenP S.Colon

listP :: IDocParser m b itemType -> (Vector itemType -> listType) -> IDocParser m b listType
listP itemP constructor = do
  blockStarterP
  l <- constructor <$> (someTill (many newlineP >> blockEnderP) $ do
                           void $ many newlineP
                           itemP)
  blockEnderP'
  return l

orderedListP :: IDocParser m b (OrderedList m b)
orderedListP = listP orderedItemP OrderedList

unorderedListP :: IDocParser m b (UnorderedList m b)
unorderedListP = listP unorderedItemP UnorderedList

descriptionListP :: IDocParser m b (DescriptionList m b)
descriptionListP = listP descriptionItemP DescriptionList

makeLenses ''OrderedList
makeLenses ''OrderedItem
makeLenses ''UnorderedList
makeLenses ''UnorderedItem
makeLenses ''DescriptionList
makeLenses ''DescriptionItem

-- mkListItemP :: S.Token -> Bool -> ListType-> IDocParser m b (ListItem m)
-- mkListItemP starter hasLabel ty = do
--   starterP
--   lbl <- if hasLabel then
--            Just <$> (someTill doubleStarterP simpleCoreP)
--          else
--            return Nothing
--   cnt <- someV $ do
--     MP.notFollowedBy $ newlineP >> some newlineP
--     simpleCoreP
--   return $ ListItem { _liAttrs = S.AttrMap mempty
--                     , _liLabel = ListLabel <$> lbl
--                     , _liContents = cnt
--                     , _liSetID = Nothing
--                       -- FIXME: This should not be Nothing!
--                     , _liType = ty
--                     }
--   where
--     starterP = void $ tokenP starter
--     doubleStarterP = starterP >> starterP

-- -- FIXME: Add a number to each constructor for nested lists.
-- -- | The type of a `List', whether "ordered", "unordered", or "labelled".
-- data ListType = Unordered
--               | Ordered
--               | Labelled
--   deriving (Eq, Ord, Show, Data, Typeable, Generic)

-- -- FIXME: Move `_liType' from `ListItem' to `List'
-- -- | A List of things, either `Ordered', `Unordered', or `Labelled' (see
-- -- `ListType'). Represents things like lists of bullet points, numbered lists,
-- -- or lists of definitions, etc.
-- data List m = List { _listContents :: Vector (ListItem m) }
--   deriving (Eq, Ord, Show, Data, Typeable, Generic, Functor)

-- -- | A label for a `ListItem' in `Labelled' `List's.
-- newtype ListLabel m = ListLabel { unListLabel :: Vector (SimpleCore m) }
--   deriving (Eq, Ord, Show, Data, Typeable, Generic, Functor)

-- -- | A single item in a `List'. Can be `Link'ed to via its `SetID'. Currently
-- -- only contains `SimpleCore' contents. (No nested lists, I'm afraid. This
-- -- should change soon.)
-- data ListItem m = ListItem { _liAttrs :: AttrMap
--                            , _liLabel :: Maybe (ListLabel m)
--                            , _liContents :: Vector (SimpleCore m)
--                            , _liSetID :: Maybe (SetID m)
--                            , _liType :: ListType
--                            }
--   deriving (Eq, Ord, Show, Data, Typeable, Generic, Functor)

-- makeLenses ''List
-- makeLenses ''ListItem

-- instance MarkupMarkup m => ToMarkup (ListItem m) where
--   toMarkup li_ = correctListItemHolder (li_^.liType) (li_^.liLabel) $
--                  concatMap toMarkup $ li_^.liContents
--     where
--       correctListItemHolder Labelled (Just l) =
--         (\x -> (dt ! class_ "idocLabel" $ toMarkup l) ++
--                (dd ! class_ "idocLabelledItem" $ x))
--       correctListItemHolder Ordered Nothing = li ! class_ "idocOrderedItem"
--       correctListItemHolder Unordered Nothing = li ! class_ "idocUnorderedItem"
--       correctListItemHolder _ _ = fail $ printf "Failed to match pattern in ToMarkup (ListItem m)."

-- instance MarkupMarkup m => ToMarkup (ListLabel m) where
--   toMarkup (ListLabel ll_) = concatMap toMarkup ll_

-- instance MarkupMarkup m => ToMarkup (List m) where
--   toMarkup (List l) = correctListHolder ((V.head l)^.liType) $
--                       concatMap toMarkup l
--     where
--       correctListHolder Unordered = ul ! class_ "idocUnorderedList"
--       correctListHolder Ordered = ol ! class_ "idocOrderedList"
--       correctListHolder Labelled = dl ! class_ "idocLabelledList"

-- instance Markupy m => Texy (List m) where
--   texy (List li_) = enumerate $
--                     concatMap (\li'_ ->
--                                   mLabel (li'_^.liSetID) $
--                                   L.item (textbf <$> texy <$> (li'_^.liLabel)) ++ (vectorTexy $ li'_^.liContents)) li_

-- instance Markupy m => Texy (ListLabel m) where
--   texy (ListLabel ll_) = vectorTexy ll_

-- instance CheckLinks m b m => CheckLinks m b (List m) where
--   checkLinks constraints container (List list_) =
--     concatMap (\listItem ->
--                   (maybe mempty (checkLinks constraints container) (listItem^.liLabel))
--                   ++ (concatMap (checkLinks constraints container) (listItem^.liContents))) list_

-- instance CheckLinks m b m => CheckLinks m b (ListLabel m) where
--   checkLinks constraints container (ListLabel llc) =
--     concatMap (checkLinks constraints container) llc

-- -- | Lists

-- unorderedItemP :: IDocParser m b (ListItem m)
-- unorderedItemP = mkListItemP S.Dash False Unordered MP.<?> "An Unordered List Item"

-- orderedItemP :: IDocParser m b (ListItem m)
-- orderedItemP = mkListItemP S.Period False Ordered MP.<?> "An Ordered List Item"

-- labelledItemP :: IDocParser m b (ListItem m)
-- labelledItemP = mkListItemP S.Colon True Labelled MP.<?> "A Labelled List Item"

-- listP :: IDocParser m b (List m)
-- listP = MP.label "A List" $ List <$> (MP.try (sepEndBy1V unorderedItemP (some newlineP))
--                                  <|>  MP.try (sepEndBy1V orderedItemP   (some newlineP))
--                                  <|>         (sepEndBy1V labelledItemP  (some newlineP)))

