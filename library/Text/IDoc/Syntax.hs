{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveFunctor #-}
-- | Syntax.hs
--
-- Author: Matt Walker
--
-- License: https://opensource.org/licenses/BSD-2-Clause
--
-- Created: Aug 24, 2017
--
-- Summary: Defines the syntax tree of idoc markup.

module Text.IDoc.Syntax where

import ClassyPrelude as CP hiding (span)

import Data.Data

import Data.Vinyl
import Data.Vinyl.CoRec
import Data.Vinyl.TypeLevel
import Data.Vinyl.Functor

import qualified Data.Vector as V

import Text.Blaze.Html5 as B
import Text.Blaze.Html5.Attributes as A

import Text.LaTeX as L hiding ((<>))
import Text.LaTeX.Base.Class
import Text.LaTeX.Packages.AMSMath as M hiding (to)
import Text.LaTeX.Packages.Hyperref as H

import Text.IDoc.Render.Tex

import Text.Megaparsec.Stream
import Text.Megaparsec.Pos

import Text.Printf

import Control.Lens hiding (cons, List)

-- * Syntax
--
-- | Everything in this file is defined Data, Typeable, and Generic, as
-- well as the usual Eq, Show, and Ord.

-- | Type synonym for keeping track of which row we are on.
type Row = Int

-- | Type synonym for keeping track of which column we are on.
type Col = Int

-- | The current debug information kept around so that we can tell the
-- user where an error occured.  More can be added later without
-- breaking much code.
data DebugInfo = DebugInfo { _diStart :: !(Row, Col)
                           , _diEnd :: !(Row, Col)
                           }
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

-- | A `Token' with attached debug information; the parser never sees
-- the debug information directly and so doesn't need to worry about
-- it.
data DebugToken d = DebugToken { _dtInfo :: d
                               , _dtToken :: Text.IDoc.Syntax.Token
                               }
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

-- | Type synonym for `DebugToken' instantiated on our currently used
-- `DebugInfo'
type DToken = DebugToken DebugInfo

-- | The type of Tokens in idoc.
data Token =
  -- "regular" text
    TextT Text
  -- symbols and punctuation
  | Equals
  | LAngle
  | RAngle
  | LBracket
  | RBracket
  | LBrace
  | RBrace
  | Colon
  | Newline
  | Dash
  | AtSign
  | BackTick
  | Asterisk
  | Underscore
  | Octothorpe
  | DoubleQuote
  | Tilde
  | Caret
  | FSlash
  | Comma
  | Period
  | DollarSign
  | PercentSign
  | SemiColon
  | BSlash
  | Plus
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

unToken :: Text.IDoc.Syntax.Token -> Text
unToken (TextT x) = x
unToken Equals = "="
unToken LAngle = "<"
unToken RAngle = ">"
unToken LBracket = "["
unToken RBracket = "]"
unToken LBrace = "{"
unToken RBrace = "}"
unToken Colon = ":"
unToken Newline = "\n"
unToken Dash = "-"
unToken AtSign = "@"
unToken BackTick = "`"
unToken Asterisk = "*"
unToken Underscore = "_"
unToken Octothorpe = "#"
unToken DoubleQuote = "\""
unToken Tilde = "~"
unToken Caret = "^"
unToken FSlash = "/"
unToken Comma = ","
unToken Period = "."
unToken DollarSign = "$"
unToken PercentSign = "%"
unToken SemiColon = ";"
unToken BSlash = "\\"
unToken Plus = "+"

-- | Newtype around a Vector of `DToken's; represents lexed source.
newtype IDocTokenStream = IDocTokenStream { unStream :: Vector DToken }

-- | Megaparsec Stream instance so that this properly works with the
-- rest of the library.
instance Stream IDocTokenStream where
  type Token IDocTokenStream = DToken
  type Tokens IDocTokenStream = Vector DToken
  tokensToChunk _ = fromList
  chunkToTokens _ = toList
  chunkLength _ = length
  advance1 _ _ (SourcePos {sourceName = sn}) (DebugToken { _dtInfo = info }) =
    let (r, c) = _diEnd info
    in
      SourcePos { sourceName = sn
                , sourceLine = mkPos $ fromIntegral r
                , sourceColumn = mkPos $ fromIntegral c
                }
  advanceN pxy p_ sp ts = advance1 pxy p_ sp (V.last ts)
  take1_ (IDocTokenStream ts) = if V.null ts then
                                  Nothing
                                else
                                  Just (V.head ts, IDocTokenStream $ V.tail ts)
  takeN_ n its | n <= 0 = Just (V.empty, its)
  takeN_ _ (IDocTokenStream ts) | V.null ts = Nothing
  takeN_ n (IDocTokenStream ts) = Just $ IDocTokenStream <$> (V.splitAt n ts)
  takeWhile_ p_ (IDocTokenStream ts) = IDocTokenStream <$> (V.span p_ ts)

-- | One of a `SimpleCore' or a `ComplexCore'; holds most -- constructs in the language.
data Core m b = SC (SimpleCore m)
              | CC (ComplexCore m b)
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

-- | Sum type for holding `Text', `QText' ("quoted text"), `Link's,
-- `InlineMath' or `Markup'.  Used inside `Paragraph's and titles (like
-- `Section' headings and so on).
data SimpleCore m =
    TextC Text
  | QTextC (QText m)
  | LinkC (Link m)
  | InlineMathC (InlineMath m)
  | MarkupC (Text.IDoc.Syntax.Markup m)
  deriving (Eq, Ord, Show, Data, Typeable, Generic, Functor)

-- | Sum type for holding the major organizing constructs of the
-- language: `List's, `Block's and `Paragraph's.
data ComplexCore m b =
    ListC (List m)
  | BlockC (Block m b)
  | ParagraphC (Paragraph m)
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

-- | A single paragraph.  Can have a `SetID'.
data Paragraph m = Paragraph { _paraContents :: Vector (SimpleCore m)
                             , _paraSetID :: Maybe (SetID m)
                             }
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

-- | The title of a `Doc'.
newtype DocTitle m = DocTitle { _unDocTitle :: Vector (SimpleCore m) }
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

-- | A single parsed idoc document.  Its `Section's will be non-empty
-- if parsed by the parser.
data Doc m b = Doc { _docTitle :: DocTitle m
                   , _docSections :: Vector (Section m b)
                   , _docSetID :: Maybe (SetID m) }
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

-- | Different types of emphasis text.  Used in `QText'.
data TextType = Strong
              | Emphasis
              | Monospace
              | Superscript
              | Subscript
              | Quoted
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

-- | `Text' that is emphasized or changed in some way (such as being
-- superscripted).
data QText m = QText { _qtText :: Vector (SimpleCore m)
                     , _qtType :: TextType
                     }
  deriving (Eq, Ord, Show, Data, Typeable, Generic, Functor)

-- | An `ID' given to an object so that it can be referred to later.
data SetID m = SetID { _sidName :: IDHash }
  deriving (Eq, Ord, Show, Data, Typeable, Generic, Functor)

-- | The "hash" part of an `ID'.  It's the part that comes after the
-- octothorpe (#).
newtype IDHash = IDHash { _unIDHash :: Text }
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

instance ToValue IDHash where
  toValue (IDHash h) = toValue $ "#" ++ h

instance Texy IDHash where
  texy (IDHash h) = raw h

-- | The type that corresponds to "attribute lists" in the idoc
-- language.
data AttrMap = AttrMap { _amMap :: Map AttrName (Maybe AttrValue) }
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

-- | Wrapper around `Text' for attribute names.
newtype AttrName = AttrName Text deriving (Eq, Ord, Show, Data, Typeable, Generic)

-- | Wrapper around `Text' for attribute values.  Might become a sum
-- type later.
newtype AttrValue = AttrValue Text deriving (Eq, Ord, Show, Data, Typeable, Generic)

-- | Sum type representing what type of link it is, an "ilink", a
-- "blink" or, an "olink".
data LinkType = Internal
              | Back Text -- ^ Relative base
              | Out (Maybe Text) -- ^ Relative base; assumed absolute if
                                 -- missing.
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

-- | The displayed text of a `Link'.
newtype LinkText m = LinkText { _unLinkText :: Vector (SimpleCore m) }
  deriving (Eq, Ord, Show, Data, Typeable, Generic, Functor)

-- | A Link around (or out of) a `Doc'.  See `LinkType' for the types
-- of possible links.  See `ID' for the format of links.
data Link m = Link { _linkText :: Maybe (LinkText m)
                   , _linkAttrs :: AttrMap
                   , _linkLocation :: ID
                   , _linkType :: LinkType
                   }
  deriving (Eq, Ord, Show, Data, Typeable, Generic, Functor)

-- | A reference to either an external website or a `SetID' somewhere
-- in this or another `Doc'.  Protocol is usually "https:\/\/", but
-- can also be "youtube:\/\/" or "image:\/\/" in certain cases (see
-- `YouTube' and `Image' `Block's).
data ID = ID { _idProtocol :: Maybe Protocol
             , _idBase :: Vector IDBase
             , _idHash :: Maybe IDHash
             }
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

-- | Type representing a protocol for an `ID'.
newtype Protocol = Protocol Text deriving (Eq, Ord, Show, Data, Typeable, Generic)

-- | The "base" of an `ID' is considered the part /after/ the
-- `Protocol' but /before/ the `IDHash'.  So in
-- "https:\/\/www.independentlearning.science\/tiki\/ArticleName#myId",
-- the IDBase would be
-- "www.independentlearning.science\/tiki\/ArticleName".
newtype IDBase = IDBase { _unIDBase :: Text } deriving (Eq, Ord, Show, Data, Typeable, Generic)

-- FIXME: Add a number to each constructor for nested lists.
-- | The type of a `List', whether "ordered", "unordered", or
-- "labelled".
data ListType = Unordered
              | Ordered
              | Labelled
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

-- FIXME: Move `_liType' from `ListItem' to `List'
-- | A List of things, either `Ordered', `Unordered', or `Labelled'
-- (see `ListType').  Represents things like lists of bullet points,
-- numbered lists, or lists of definitions, etc.
data List m = List { _listContents :: Vector (ListItem m) }
  deriving (Eq, Ord, Show, Data, Typeable, Generic, Functor)

-- | A label for a `ListItem' in `Labelled' `List's.
newtype ListLabel m = ListLabel { unListLabel :: Vector (SimpleCore m) }
  deriving (Eq, Ord, Show, Data, Typeable, Generic, Functor)

-- | A single item in a `List'.  Can be `Link'ed to via its `SetID'.
-- Currently only contains `SimpleCore' contents.  (No nested lists,
-- I'm afraid.  This should change soon.)
data ListItem m = ListItem { _liAttrs :: AttrMap
                           , _liLabel :: Maybe (ListLabel m)
                           , _liContents :: Vector (SimpleCore m)
                           , _liSetID :: Maybe (SetID m)
                           , _liType :: ListType
                           }
  deriving (Eq, Ord, Show, Data, Typeable, Generic, Functor)

-- | Inline markup of text.  See `MarkupType' for the valid values of
-- `_muType'.  May contain a non-empty `AttrMap'.
data Markup m = Markup { _muType :: m
                       , _muAttrs :: AttrMap
                       , _muSetID :: Maybe (SetID m)
                       }
  deriving (Eq, Ord, Show, Data, Typeable, Generic, Functor)

class MarkupMarkup m where
  markupMarkup :: AttrMap -> Maybe (SetID n) -> m -> Html

class Markupy m where
  markupy :: (LaTeXC l, Markupy n) => AttrMap -> Maybe (SetID n) -> m -> l

-- | Inline math, LaTeX style.  May have an attached `AttrMap' or
-- `SetID'.  Contents are unparsed `Token's.
data InlineMath m = InlineMath { _imAttrs    :: AttrMap
                               , _imContents :: Vector Text.IDoc.Syntax.Token
                               , _imSetID    :: Maybe (SetID m)
                               }
  deriving (Eq, Ord, Show, Data, Typeable, Generic, Functor)

-- | Newtype for titles of `Block's.
newtype BlockTitle m = BlockTitle { unBlockTitle :: Vector (SimpleCore m) }
  deriving (Eq, Ord, Show, Data, Typeable, Generic, Functor)

-- | Common wrapper for blocks.  Most of the interesting parts will be
-- found inside `_bType', which is usually a sum type (either a `CoRec' or
-- just a regular ADT).
data Block m b = Block { _bType  :: b m
                       , _bAttrs :: AttrMap
                       , _bTitle :: Maybe (BlockTitle m)
                       , _bSetID :: Maybe (SetID m)
                       }
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

class BlockMarkup m b where
  blockMarkup :: AttrMap -> Maybe (BlockTitle m) -> Maybe (SetID m) -> b -> Html

class Blocky m b where
  blocky :: LaTeXC l => AttrMap -> Maybe (BlockTitle m) -> Maybe (SetID m) -> b -> l

data SectionType = Preamble
                 | TopSection
                 | SubSection
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

newtype SectionTitle m = SectionTitle { _unSectionTitle :: Vector (SimpleCore m) }
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

data Section m b = Section { _secType     :: SectionType
                           , _secAttrs    :: AttrMap
                           , _secContents :: Vector (Core m b)
                           , _secTitle    :: (SectionTitle m)
                           , _secSetID    :: Maybe (SetID m)
                           }
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

-- * Lenses
--
-- | We then define lenses for nearly everything in the module.

makeLenses ''DebugInfo
makeLenses ''DebugToken
makeLenses ''AttrMap
makeLenses ''QText
makeLenses ''SetID
makeLenses ''Section
makeLenses ''LinkText
makeLenses ''Link
makeLenses ''List
makeLenses ''ListItem
makeLenses ''Text.IDoc.Syntax.Markup
makeLenses ''InlineMath
makeLenses ''Block
makeLenses ''Doc
makeLenses ''Paragraph
makeLenses ''ID
makeLenses ''DocTitle
makeLenses ''SectionTitle
makeLenses ''IDBase
makeLenses ''IDHash

instance (BlockMarkup m (b m), MarkupMarkup m) => ToMarkup (Section m b) where
  toMarkup s = B.section ! class_ "idocSection" $
               titlify $ concatMap toMarkup $ s^.secContents
    where titlify = case s^.secType of
            Preamble -> CP.id
            TopSection -> (mID (s^.secSetID)
                            (h2 ! class_ "idocTopSectionTitle clearfix" $
                             s^.secTitle.to toMarkup) ++)
            SubSection -> (mID (s^.secSetID)
                            (h3 ! class_ "idocSubSectionTitle clearfix" $
                             s^.secTitle.to toMarkup) ++)

instance MarkupMarkup m => ToMarkup (SectionTitle m) where
  toMarkup (SectionTitle st) = concatMap toMarkup st

instance MarkupMarkup m => ToMarkup (BlockTitle m) where
  toMarkup (BlockTitle bt) = concatMap toMarkup bt

instance MarkupMarkup m => ToMarkup (InlineMath m) where
  toMarkup im = (mID (im^.imSetID) (B.span $ ("\\(" ++ concatMap toMarkup (im^.imContents) ++ "\\)"))) ! class_ "idocInlineMath"

instance MarkupMarkup m => ToMarkup (Text.IDoc.Syntax.Markup m) where
  toMarkup mu_ = markupMarkup (mu_^.muAttrs) (mu_^.muSetID) (mu_^.muType)

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

instance MarkupMarkup m => ToMarkup (Link m) where
  toMarkup l = a ! class_ (toValue $ l^.linkType)
                 ! A.href (toValue $ l) $
                 maybe (l^.to toText.to toMarkup) toMarkup (l^.linkText)

toText :: Link m -> Text
toText l =
  let id_ = l^.linkLocation
  in
    case l^.linkType of
      Out rel_ -> let (proto, hash_) =
                        case (id_^.idProtocol, id_^.idHash) of
                          (Just (Protocol p_), Just (IDHash h)) -> (p_ ++ "://", h)
                          (Just (Protocol p_), Nothing) -> (p_ ++ "://", "")
                          _ -> error $ "Invalid Outlink:\nProtocol: " ++ (show $ id_^.idProtocol) ++ "\nHash: " ++ (show $ id_^.idHash)
                  in
                    (maybe proto CP.id rel_) ++
                    (concatMap _unIDBase $ intersperse (IDBase "/") (id_^.idBase)) ++
                    (if hash_ /= "" then "#" ++ hash_ else "")
      Internal -> case id_^.idHash of
                    (Just (IDHash h)) -> "#" ++ h
                    _ -> "WTF: ToValue (Link m)"
      Back rel_ -> rel_ ++
                   (concatMap _unIDBase $ intersperse (IDBase "/") (id_^.idBase)) ++
                   (case id_^.idHash of
                      (Just (IDHash h)) -> "#" ++ h
                      _ -> mempty)

instance ToValue (Link m) where
  toValue l = toValue $ toText l

instance ToValue LinkType where
  toValue Internal = "idocInternal"
  toValue (Back _) = "idocBackLink"
  toValue (Out _) = "idocOutLink"

instance MarkupMarkup m => ToMarkup (LinkText m) where
  toMarkup (LinkText lt) = concatMap toMarkup lt

instance ToMarkup Text.IDoc.Syntax.Token where
  toMarkup t = toMarkup $ unToken t

instance MarkupMarkup m => ToValue (SetID m) where
  toValue (SetID { _sidName = (IDHash sid) }) = toValue sid

instance MarkupMarkup m => ToMarkup (QText m) where
  toMarkup qt = decorateTextWith (qt^.qtType) $
                concatMap toMarkup $ qt^.qtText
    where
      decorateTextWith Strong x = strong ! class_ "idocStrong" $ x
      decorateTextWith Emphasis x = em ! class_ "idocEmphasis" $ x
      decorateTextWith Monospace x = B.span ! class_ "idocMonospace" $ x
      decorateTextWith Superscript x = sup ! class_ "idocSuperscript" $ x
      decorateTextWith Subscript x = sub ! class_ "idocSubscript" $ x
      decorateTextWith Quoted x = q ! class_ "idocQuoted" $ x

instance (MarkupMarkup m, BlockMarkup m (b m)) => ToMarkup (Doc m b) where
  toMarkup d = B.article ! class_ "idocDoc" $
               (toMarkup $ d^.docTitle) ++
               (concatMap toMarkup $ d^.docSections)

instance (MarkupMarkup m, BlockMarkup m (b m)) => ToMarkup (Core m b) where
  toMarkup (SC sc) = toMarkup sc
  toMarkup (CC cc) = toMarkup cc

instance MarkupMarkup m => ToMarkup (SimpleCore m) where
  toMarkup (TextC t) = toMarkup t
  toMarkup (QTextC qt) = toMarkup qt
  toMarkup (LinkC l) = toMarkup l
  toMarkup (InlineMathC im) = toMarkup im
  toMarkup (MarkupC m) = toMarkup m

instance MarkupMarkup m => ToMarkup (Paragraph m) where
  toMarkup p_ = p ! class_ "idocParagraph" $
                concatMap toMarkup $ p_^.paraContents

instance (MarkupMarkup m, BlockMarkup m (b m)) => ToMarkup (ComplexCore m b) where
  toMarkup (ListC l) = toMarkup l
  toMarkup (BlockC b_) = toMarkup b_
  toMarkup (ParagraphC p_) = toMarkup p_

instance (MarkupMarkup m, BlockMarkup m (b m)) => ToMarkup (Block m b) where
  toMarkup b_ = blockMarkup (b_^.bAttrs) (b_^.bTitle) (b_^.bSetID) (b_^.bType)

instance MarkupMarkup m => ToMarkup (DocTitle m) where
  toMarkup (DocTitle dt_) = h1 ! class_ "idocDocTitle" $
                            concatMap toMarkup dt_

instance ( RecApplicative xs
         , MarkupMarkup m
         , AllAllSat '[BlockMarkup m] xs) =>
  BlockMarkup m (CoRec Data.Vinyl.Functor.Identity xs) where
  blockMarkup a_ t s x = getIdentity $ onCoRec (Proxy :: Proxy '[BlockMarkup m]) (blockMarkup a_ t s) x

instance ( RecApplicative xs
         , AllAllSat '[MarkupMarkup] xs) =>
  MarkupMarkup (CoRec Data.Vinyl.Functor.Identity xs) where
  markupMarkup a_ s x = getIdentity $ onCoRec (Proxy :: Proxy '[MarkupMarkup]) (markupMarkup a_ s) x

-- * Some Helper Functions
vectorBlockToMarkup :: B.ToMarkup a =>
                       B.AttributeValue -- ^ Html class
                    -> (B.Html -> B.Html) -- ^ decorator
                    -> Vector a -- ^ vector block
                    -> B.Html
vectorBlockToMarkup cls dec vb = B.div B.! A.class_ cls $
                                 dec $
                                 concatMap B.toMarkup
                                 vb

verbatimBlockToMarkup :: B.AttributeValue -- ^ Html class
                      -> (B.Html -> B.Html) -- ^ decorator
                      -> Vector Text.IDoc.Syntax.Token -- ^ verbatim block
                      -> B.Html
verbatimBlockToMarkup cls dec vb = B.div B.! A.class_ cls $
                                   dec $
                                   concatMap (\x -> if x == Newline then
                                                      B.toMarkup B.br
                                                    else
                                                      B.toMarkup x)
                                   vb

newtype LinkLevel = LinkLevel Int
  deriving (Eq, Ord, Show)

-- -- | Dear god, don't look at the definition of this.
-- listLinks :: Doc m b -> Vector (SetID m, LinkLevel)
-- listLinks (Doc { _docSections = ss
--                , _docSetID = dsid }) =
--   catMaybes (singleton ((\x -> (x, LinkLevel 1)) <$> dsid)) <>
--   concatMap (\(Section { _secSetID = ssid
--                        , _secContents = scnts }) ->
--                 catMaybes $ ((\case
--                                 CC (BlockC (Block { _bSetID = bsid })) -> do
--                                   sid <- bsid
--                                   return (sid, LinkLevel 3)
--                                 CC (ParagraphC (Paragraph { _paraSetID = psid })) -> do
--                                   sid <- psid
--                                   return (sid, LinkLevel 3)
--                                 _ -> Nothing
--                                 -- FIXME: Add lists here
--                             ) <$> scnts) <> singleton ((\x -> (x, LinkLevel 2)) <$> ssid)) ss

listLinks :: Doc m b -> Protocol -> Vector IDBase -> Vector (Link m, LinkLevel)
listLinks d proto rel_ = singleton (docLink, LinkLevel 1) <> sectionLinks
  where
    docLink = Link { _linkText = Just $ LinkText $ d^.docTitle.unDocTitle
                   , _linkAttrs = AttrMap CP.mempty
                   , _linkLocation = ID { _idProtocol = Just proto
                                        , _idBase = rel_
                                        , _idHash = Just $ IDHash "" }
                   , _linkType = Internal
                   }
    sectionLinks = (\s -> ( Link { _linkText = Just $ LinkText $ s^.secTitle.unSectionTitle
                                 , _linkAttrs = AttrMap CP.mempty
                                 , _linkLocation = ID { _idProtocol = Just proto
                                                      , _idBase = rel_
                                                      , _idHash = _sidName <$> (s^.secSetID)
                                                      }
                                 , _linkType = Internal
                                 }
                          , LinkLevel 2)) <$> (d^.docSections)

mID :: ToValue (SetID m) => Maybe (SetID m) -> (Html -> Html)
mID mid = case mid of
  Nothing -> CP.id
  Just id_ -> (\x -> x ! A.id (toValue id_))

mLabel :: (LaTeXC l, Texy (SetID m)) => Maybe (SetID m) -> (l -> l)
mLabel mid = case mid of
  Nothing -> CP.id
  Just id_ -> ((texy id_) ++)

mTitleT :: (LaTeXC l, Texy (BlockTitle m)) => Maybe (BlockTitle m) -> Text -> l
mTitleT mbt defaultTitle = maybe (texy defaultTitle) texy mbt

type LIcon = LaTeX

instance (Markupy m, Blocky m (b m)) => Texy (Doc m b) where
  texy d = chapter (mLabel (d^.docSetID) $ (texy $ d^.docTitle)) ++
           (vectorTexy $ d^.docSections)

instance (Markupy m, Blocky m (b m)) => Texy (Section m b) where
  texy s = starter (mLabel (s^.secSetID) $ texy $ s^.secTitle) ++
           (vectorTexy $ s^.secContents)
    where starter = case s^.secType of
            Preamble -> const ""
            TopSection -> L.section
            SubSection -> L.subsection

instance Markupy m => Texy (SectionTitle m) where
  texy (SectionTitle s) = vectorTexy s

instance Markupy m => Texy (DocTitle m) where
  texy (DocTitle dt_) = vectorTexy dt_

instance (Markupy m, Blocky m (b m)) => Texy (Core m b) where
  texy (SC sc) = texy sc
  texy (CC cc) = texy cc

instance Markupy m => Texy (SimpleCore m) where
  texy (TextC t) = texy t
  texy (QTextC qt) = texy qt
  texy (LinkC l) = texy l
  texy (InlineMathC im) = texy im
  texy (MarkupC m) = texy m

instance (Markupy m, Blocky m (b m)) => Texy (ComplexCore m b) where
  texy (ListC l) = texy l
  texy (BlockC b_) = texy b_
  texy (ParagraphC p_) = texy p_

instance Markupy m => Texy (QText m) where
  texy qt = decorateTextWith (qt^.qtType) $
            concatMap texy $ qt^.qtText
    where
      decorateTextWith Strong = textbf
      decorateTextWith Emphasis = emph
      decorateTextWith Monospace = texttt
      decorateTextWith Superscript = textsuperscript
      decorateTextWith Subscript = textsubscript
      decorateTextWith Quoted = qts

      textsuperscript x = between x (raw "\textsuperscript{") (raw "}")

      textsubscript x = between x (raw "\textsubscript{") (raw "}")

instance Markupy m => Texy (SetID m) where
  texy (SetID { _sidName = IDHash sid }) = L.label $ texy sid

instance Markupy m => Texy (Link m) where
  texy l = case l^.linkType of
             Out rel_ -> H.href []
                         (createURL $ unpack $ fromOut (l^.linkLocation) rel_)
                         (maybe (l^.to toText.to texy) texy (l^.linkText))
             Internal -> hyperref' (fromInternal $ l^.linkLocation)
                                   (maybe (l^.to toText.to texy) texy (l^.linkText))
             Back rel_ -> H.href []
                         (createURL $ unpack $ fromBack (l^.linkLocation) rel_)
                         (maybe (l^.to toText.to texy) texy (l^.linkText))
    where
      -- TODO: Make sense of this with rel_ being passed in.
      fromOut id_ _ =
        let (proto, hash_) =
              case (id_^.idProtocol, id_^.idHash) of
                (Just (Protocol p_), Just (IDHash h)) ->
                  (p_ ++ "://", h)
                (Just (Protocol p_), Nothing) ->
                  (p_ ++ "://", "")
                _ -> error $ "Invalid Out Link:\nProtocol: " ++ (show $ id_^.idProtocol) ++ "\nHash: " ++ (show $ id_^.idHash)
        in
          proto ++
          (concatMap _unIDBase $ intersperse (IDBase "/") (id_^.idBase)) ++ 
          "#" ++ hash_

      fromInternal id_ = case id_^.idHash of
                           Just (IDHash h) -> h
                           _ -> error "Invalid Internal Link: No hash provided."

      fromBack id_ rel_ = rel_ ++
                          (concatMap _unIDBase $ intersperse (IDBase "/") (id_^.idBase))

instance Markupy m => Texy (LinkText m) where
  texy (LinkText lt) = concatMap texy lt

instance Markupy m => Texy (InlineMath m) where
  texy im = M.math $ concatMap texy $ im^.imContents

instance Texy Text.IDoc.Syntax.Token where
  texy t = raw $ unToken t

instance Markupy m => Texy (Text.IDoc.Syntax.Markup m) where
  texy mu_ = markupy (mu_^.muAttrs) (mu_^.muSetID) (mu_^.muType)

  -- mLabel (mu_^.muSetID) $
  --   case (mu_^.muType) of
  --     Footnote -> footnote $ concatMap texy $ mu_^.muContents
  --     FootnoteRef -> ref $ concatMap texy $ mu_^.muContents
  --     Citation -> L.cite $ concatMap texy $ mu_^.muContents

instance Markupy m => Texy (Paragraph m) where
  texy p_ = (concatMap texy $ p_^.paraContents) ++ "\n\n"

instance Markupy m => Texy (BlockTitle m) where
  texy (BlockTitle bt) = concatMap texy bt

instance Markupy m => Texy (List m) where
  texy (List li_) = enumerate $
                     concatMap (\li'_ ->
                                  mLabel (li'_^.liSetID) $ 
                                  L.item (textbf <$> texy <$> (li'_^.liLabel)) ++ (vectorTexy $ li'_^.liContents)) li_

instance Markupy m => Texy (ListLabel m) where
  texy (ListLabel ll_) = vectorTexy ll_

instance Blocky m (b m) => Texy (Block m b) where
  texy b_ = blocky (b_^.bAttrs) (b_^.bTitle) (b_^.bSetID) (b_^.bType)

-- Type-level magic!
instance ( RecApplicative xs
         , Markupy m
         , AllAllSat '[Blocky m] xs) =>
  Blocky m (CoRec Data.Vinyl.Functor.Identity xs) where
  blocky a_ t s x = getIdentity $ onCoRec (Proxy :: Proxy '[Blocky m]) (blocky a_ t s) x

instance ( RecApplicative xs
         , AllAllSat '[Markupy] xs) =>
  Markupy (CoRec Data.Vinyl.Functor.Identity xs) where
  markupy a_ s x = getIdentity $ onCoRec (Proxy :: Proxy '[Markupy]) (markupy a_ s) x
