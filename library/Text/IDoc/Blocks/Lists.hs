{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE UndecidableInstances #-}

module Text.IDoc.Blocks.Lists where

import ClassyPrelude as CP
import Control.Lens hiding (List)
import Data.Data

import Text.Blaze.Html5 as B
import Text.Blaze.Html5.Attributes as A
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
  blockMarkup a_ t s (OListB ol_) = blockMarkup a_ t s ol_
  blockMarkup a_ t s (UListB ul_) = blockMarkup a_ t s ul_
  blockMarkup a_ t s (DListB dl_) = blockMarkup a_ t s dl_

listMarkup :: ( MarkupMarkup m
              , MonoFoldable mono
              , ToMarkup (Element mono) ) =>
              Maybe (SetID m) -> mono -> Html
listMarkup mSetID cnts =
  (maybe CP.id (\i_ -> (B.! A.id (B.toValue i_))) mSetID) $
  (B.div B.! A.class_ ("mb-3 list-group idocList") $
         concatMap toMarkup cnts)

instance (MarkupMarkup m, BlockMarkup m (b m)) => BlockMarkup m (OrderedList m b) where
  blockMarkup _ _ s (OrderedList cnts) =
    (maybe CP.id (\i_ -> (B.! A.id (B.toValue i_))) s) $
    (B.ol B.! A.class_ ("mb-3 idocList idocOrderedList") $
          concatMap toMarkup cnts)

instance (MarkupMarkup m, BlockMarkup m (b m)) => ToMarkup (OrderedItem m b) where
  toMarkup (OrderedItem cnts) =
    (B.li B.! A.class_ "idocListItem idocOrderedListItem") $
          concatMap toMarkup cnts

instance (MarkupMarkup m, BlockMarkup m (b m)) => BlockMarkup m (UnorderedList m b) where
  blockMarkup _ _ s (UnorderedList cnts) =
    (maybe CP.id (\i_ -> (B.! A.id (B.toValue i_))) s) $
    (B.ul B.! A.class_ ("mb-3 idocList idocOrderedList") $
          concatMap toMarkup cnts)

instance (MarkupMarkup m, BlockMarkup m (b m)) => ToMarkup (UnorderedItem m b) where
  toMarkup (UnorderedItem cnts) =
    (B.li B.! A.class_ "idocListItem idocUnorderedListItem") $
          concatMap toMarkup cnts

instance (MarkupMarkup m, BlockMarkup m (b m)) => BlockMarkup m (DescriptionList m b) where
  blockMarkup _ _ s (DescriptionList cnts) =
    (maybe CP.id (\i_ -> (B.! A.id (B.toValue i_))) s) $
    (B.ul B.! A.class_ ("mb-3 idocList idocDescriptionList") $
          concatMap toMarkup cnts)

instance (MarkupMarkup m, BlockMarkup m (b m)) => ToMarkup (DescriptionItem m b) where
  toMarkup (DescriptionItem label_ cnts) =
    ((B.dt B.! A.class_ "idocDescriptionListLabel") $
           concatMap toMarkup label_) <>
    ((B.dd B.! A.class_ "idocDescriptionListDescription") $
           concatMap toMarkup cnts)

instance (Markupy m, Blocky m (b m)) => Blocky m (ListB m b) where
  blocky a_ bt_ msid (OListB ol_) = blocky a_ bt_ msid ol_
  blocky a_ bt_ msid (UListB ul_) = blocky a_ bt_ msid ul_
  blocky a_ bt_ msid (DListB dl_) = blocky a_ bt_ msid dl_

instance (Markupy m, Blocky m (b m)) => Blocky m (OrderedList m b) where
  blocky _ _ msid (OrderedList cnts) =
    mLabel msid $ enumerate $
    vectorTexy cnts

instance (Markupy m, Blocky m (b m)) => Texy (OrderedItem m b) where
  texy (OrderedItem cnts) = L.item Nothing ++ vectorTexy cnts

instance (Markupy m, Blocky m (b m)) => Blocky m (UnorderedList m b) where
  blocky _ _ msid (UnorderedList cnts) =
    mLabel msid $ itemize $
    vectorTexy cnts

instance (Markupy m, Blocky m (b m)) => Texy (UnorderedItem m b) where
  texy (UnorderedItem cnts) = L.item Nothing ++ vectorTexy cnts

instance (Markupy m, Blocky m (b m)) => Blocky m (DescriptionList m b) where
  blocky _ _ msid (DescriptionList cnts) =
    mLabel msid $ description $
    vectorTexy cnts

instance (Markupy m, Blocky m (b m)) => Texy (DescriptionItem m b) where
  texy (DescriptionItem label_ cnts) =
    L.item (Just $ vectorTexy label_) ++ vectorTexy cnts

instance (CheckLinks m b m, CheckLinks m b (b m)) => CheckLinks m b (ListB m b) where
  checkLinks constraints container (OListB ol_) =
    checkLinks constraints container ol_
  checkLinks constraints container (UListB ul_) =
    checkLinks constraints container ul_
  checkLinks constraints container (DListB dl_) =
    checkLinks constraints container dl_

instance (CheckLinks m b m, CheckLinks m b (b m)) => CheckLinks m b (OrderedList m b) where
  checkLinks constraints container (OrderedList cnts) =
    concatMap (checkLinks constraints container) cnts

instance (CheckLinks m b m, CheckLinks m b (b m)) => CheckLinks m b (OrderedItem m b) where
  checkLinks constraints container (OrderedItem cnts) =
    concatMap (checkLinks constraints container) cnts

instance (CheckLinks m b m, CheckLinks m b (b m)) => CheckLinks m b (UnorderedList m b) where
  checkLinks constraints container (UnorderedList cnts) =
    concatMap (checkLinks constraints container) cnts

instance (CheckLinks m b m, CheckLinks m b (b m)) => CheckLinks m b (UnorderedItem m b) where
  checkLinks constraints container (UnorderedItem cnts) =
    concatMap (checkLinks constraints container) cnts

instance (CheckLinks m b m, CheckLinks m b (b m)) => CheckLinks m b (DescriptionList m b) where
  checkLinks constraints container (DescriptionList cnts) =
    concatMap (checkLinks constraints container) cnts

instance (CheckLinks m b m, CheckLinks m b (b m)) => CheckLinks m b (DescriptionItem m b) where
  checkLinks constraints container (DescriptionItem label_ cnts) =
    concatMap (checkLinks constraints container) label_++
    concatMap (checkLinks constraints container) cnts

simpleListItemP :: S.Token -> (Vector (Core m b) -> a) -> IDocParser m b a
simpleListItemP starter constructor = do
  void starterP
  contents_ <- someV $ do
    MP.notFollowedBy $ (MP.try (void $ (some newlineP) >> starterP)
                        MP.<|> (many newlineP >> blockEnderP))
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
    MP.notFollowedBy $ (MP.try (void $ (some newlineP) >> starterP)
                        MP.<|> (many newlineP >> blockEnderP))
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

