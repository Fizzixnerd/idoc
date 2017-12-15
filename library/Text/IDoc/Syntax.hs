{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
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

import qualified Text.Megaparsec.Prim as Prim
import Text.Megaparsec.Pos

import Text.Printf

import Control.Lens hiding (cons, List)

-- * Syntax
--
-- | Everything in this file is defined Data, Typeable, and Generic, as
-- well as the usual Eq, Show, and Ord.

-- | Type synonym for keeping track of which row we are on.
type Row = Word

-- | Type synonym for keeping track of which column we are on.
type Col = Word

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
                               , _dtToken :: Token
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

unToken :: Token -> Text
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
instance Prim.Stream IDocTokenStream where
  type Token IDocTokenStream = DToken
  uncons s = (fmap IDocTokenStream) <$> (CP.uncons $ unStream s)
  updatePos _ _ sp t = 
    let info = _dtInfo t
        (r1, c1) = bimap unsafePos unsafePos $ _diStart info
        (r2, c2) = bimap unsafePos unsafePos $ _diEnd info
        sp1 = sp { sourceLine = r1
                 , sourceColumn = c1
                 }
        sp2 = sp { sourceLine = r2
                 , sourceColumn = c2
                 }
        in
      (sp1, sp2)

-- | One of a `SimpleCore' or a `ComplexCore'; holds most interesting
-- constructs in the language.
data Core a = SC SimpleCore
            | CC (ComplexCore a)
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

-- | Sum type for holding `Text', `QText' ("quoted text"), `Link's,
-- `InlineMath' or `Markup'.  Used inside `Paragraph's and titles (like
-- `Section' headings and so on).
data SimpleCore =
    TextC Text
  | QTextC QText
  | LinkC Link
  | InlineMathC InlineMath
  | MarkupC Text.IDoc.Syntax.Markup
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

-- | Sum type for holding the major organizing constructs of the
-- language: `List's, `Block's and `Paragraph's.
data ComplexCore a =
    ListC List
  | BlockC (Block a)
  | ParagraphC Paragraph
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

-- | A single paragraph.  Can have a `SetID'.
data Paragraph = Paragraph { _paraContents :: Vector SimpleCore
                           , _paraSetID :: Maybe SetID
                           }
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

-- | The title of a `Doc'.
newtype DocTitle = DocTitle { unDocTitle :: Vector SimpleCore }
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

-- | A single parsed idoc document.  Its `Section's will be non-empty
-- if parsed by the parser.
data Doc a = Doc  { _docTitle :: DocTitle
                  , _docSections :: Vector (Section a)
                  , _docSetID :: Maybe SetID }
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
data QText = QText { _qtText :: Vector SimpleCore
                   , _qtType :: TextType
                   }
  deriving (Eq, Ord, Show, Data, Typeable, Generic)


-- | An `ID' given to an object so that it can be referred to later.
data SetID = SetID { _sidName :: IDHash
                   , _sidDisplay :: Vector SimpleCore }
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

-- | The "hash" part of an `ID'.  It's the part that comes after the
-- octothorpe (#).
newtype IDHash = IDHash { unIDHash :: Text } deriving (Eq, Ord, Show, Data, Typeable, Generic)

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
              | Back
              | Out
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

-- | The displayed text of a `Link'.
newtype LinkText = LinkText { unLinkText :: Vector SimpleCore }
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

-- | A Link around (or out of) a `Doc'.  See `LinkType' for the types
-- of possible links.  See `ID' for the format of links.
data Link = Link { _linkText :: LinkText
                 , _linkAttrs :: AttrMap
                 , _linkLocation :: ID
                 , _linkType :: LinkType
                 }
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

-- | A reference to either an external website or a `SetID' somewhere
-- in this or another `Doc'.  Protocol is usually "https:\/\/", but
-- can also be "youtube:\/\/" or "image:\/\/" in certain cases (see
-- `YouTube' and `Image' `Block's).
data ID = ID { _idProtocol :: Maybe Protocol
             , _idBase :: Vector IDBase
             , _idHash :: Maybe IDHash
             }
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

-- | Type representing a protocol for an `ID'.  May be changed to a
-- sum type later.
newtype Protocol = Protocol Text deriving (Eq, Ord, Show, Data, Typeable, Generic)

-- | The "base" of an `ID' is considered the part /after/ the
-- `Protocol' but /before/ the `IDHash'.  So in
-- "https:\/\/www.independentlearning.science\/tiki\/ArticleName#myId",
-- the IDBase would be
-- "www.independentlearning.science\/tiki\/ArticleName".
newtype IDBase = IDBase { unIDBase :: Text } deriving (Eq, Ord, Show, Data, Typeable, Generic)

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
data List = List { _listContents :: Vector ListItem }
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

-- | A label for a `ListItem' in `Labelled' `List's.
newtype ListLabel = ListLabel { unListLabel :: Vector SimpleCore }
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

-- | A single item in a `List'.  Can be `Link'ed to via its `SetID'.
-- Currently only contains `SimpleCore' contents.  (No nested lists,
-- I'm afraid.  This should change soon.)
data ListItem = ListItem { _liAttrs :: AttrMap
                         , _liLabel :: Maybe ListLabel
                         , _liContents :: Vector SimpleCore
                         , _liSetID :: Maybe SetID
                         , _liType :: ListType
                         }
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

-- | Sum type for the different kinds of `Markup'.  "Footnotes" are
-- what you would expect.  "FootnoteRefs" are references to previous
-- footnotes via their `SetID'.  "Citations" are what you would
-- expect.
data MarkupType = Footnote
                | FootnoteRef
                | Citation
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

-- | Inline markup of text.  See `MarkupType' for the valid values of
-- `_muType'.  May contain an `AttrMap'.  Text to show (if any) is
-- held in `_muContents'.
data Markup = Markup { _muType :: MarkupType
                     , _muAttrs :: AttrMap
                     , _muContents :: Vector SimpleCore
                     , _muSetID :: Maybe SetID
                     }
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

-- | Inline math, LaTeX style.  May have an attached `AttrMap' or
-- `SetID'.  Contents are unparsed `Token's.
data InlineMath = InlineMath { _imAttrs    :: AttrMap
                             , _imContents :: Vector Token
                             , _imSetID    :: Maybe SetID
                             }
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

-- | Newtype for titles of `Block's.
newtype BlockTitle = BlockTitle { unBlockTitle :: Vector SimpleCore }
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

-- | Common wrapper for blocks.  Most of the interesting parts will be
-- found inside `_bType', which is usually a sum type (either a `CoRec' or
-- just a regular ADT).
data Block a = Block { _bType  :: a
                     , _bAttrs :: AttrMap
                     , _bTitle :: Maybe BlockTitle
                     , _bSetID :: Maybe SetID
                     }
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

class BlockMarkup a where
  blockMarkup :: AttrMap -> Maybe BlockTitle -> Maybe SetID -> a -> Html

data SectionType = Preamble
                 | TopSection
                 | SubSection
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

newtype SectionTitle = SectionTitle { unSectionTitle :: Vector SimpleCore }
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

data Section a = Section { _secType     :: SectionType
                         , _secAttrs    :: AttrMap
                         , _secContents :: Vector (Core a)
                         , _secTitle    :: SectionTitle
                         , _secSetID    :: Maybe SetID
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
makeLenses ''Link
makeLenses ''List
makeLenses ''ListItem
makeLenses ''Text.IDoc.Syntax.Markup
makeLenses ''InlineMath
makeLenses ''Block
makeLenses ''Doc
makeLenses ''Paragraph
makeLenses ''ID

instance BlockMarkup a => ToMarkup (Section a) where
  toMarkup s = section ! class_ "idocSection" $
               titlify $ concatMap toMarkup $ s^.secContents
    where titlify = case s^.secType of
            Preamble -> CP.id
            TopSection -> (mID (s^.secSetID) 
                            (h2 ! class_ "idocTopSectionTitle clearfix" $
                             s^.secTitle.to toMarkup) ++)
            SubSection -> (mID (s^.secSetID) 
                            (h3 ! class_ "idocSubSectionTitle clearfix" $
                             s^.secTitle.to toMarkup) ++)

instance ToMarkup SectionTitle where
  toMarkup (SectionTitle st) = concatMap toMarkup st

instance ToMarkup BlockTitle where
  toMarkup (BlockTitle bt) = concatMap toMarkup bt

instance ToMarkup InlineMath where
  toMarkup im = (mID (im^.imSetID) (B.span $ ("\\(" ++ concatMap toMarkup (im^.imContents) ++ "\\)"))) ! class_ "idocInlineMath"

instance ToMarkup Text.IDoc.Syntax.Markup where
  toMarkup mu = mID (mu^.muSetID) $
    case (mu^.muType) of
      Footnote -> B.span ! class_ "idocFootnote" $ 
                  concatMap toMarkup (mu^.muContents)
      FootnoteRef -> B.span ! class_ "idocFootnoteRef" $
                     concatMap toMarkup (mu^.muContents)
      Citation -> B.span ! class_ "idocCitation" $
                  concatMap toMarkup (mu^.muContents)

instance ToMarkup ListItem where
  toMarkup li_ = correctListItemHolder (li_^.liType) (li_^.liLabel) $ 
                 concatMap toMarkup $ li_^.liContents
    where
      correctListItemHolder Labelled (Just l) = 
        (\x -> (dt ! class_ "idocLabel" $ toMarkup l) ++
               (dd ! class_ "idocLabelledItem" $ x))
      correctListItemHolder Ordered Nothing = li ! class_ "idocOrderedItem"
      correctListItemHolder Unordered Nothing = li ! class_ "idocUnorderedItem"
      correctListItemHolder lt l = fail $ printf "Failed to match pattern with ListType `%s' and label `%s'." (show lt) (show l)

instance ToMarkup ListLabel where
  toMarkup (ListLabel ll) = concatMap toMarkup ll

instance ToMarkup List where
  toMarkup (List l) = correctListHolder ((V.head l)^.liType) $
                      concatMap toMarkup l
    where
      correctListHolder Unordered = ul ! class_ "idocUnorderedList"
      correctListHolder Ordered = ol ! class_ "idocOrderedList"
      correctListHolder Labelled = dl ! class_ "idocLabelledList"

instance ToMarkup ID where
  toMarkup id_ = idHelper toMarkup id_

instance ToValue ID where
  toValue id_ = idHelper toValue id_

instance ToMarkup Link where
  toMarkup l = a ! class_ (toValue $ l^.linkType)
                   ! href (toValue $ l^.linkLocation) $
                   toMarkup $ l^.linkText

instance ToValue LinkType where
  toValue Internal = "idocInternal"
  toValue Back = "idocBackLink"
  toValue Out = "idocOutLink"

instance ToMarkup LinkText where
  toMarkup (LinkText lt) = concatMap toMarkup lt

instance ToMarkup Token where
  toMarkup t = toMarkup $ unToken t

instance ToValue SetID where
  toValue (SetID { _sidName = (IDHash sid) }) = toValue sid

instance ToMarkup QText where
  toMarkup qt = decorateTextWith (qt^.qtType) $
                concatMap toMarkup $ qt^.qtText
    where
      decorateTextWith Strong x = strong ! class_ "idocStrong" $ x
      decorateTextWith Emphasis x = em ! class_ "idocEmphasis" $ x
      decorateTextWith Monospace x = B.span ! class_ "idocMonospace" $ x
      decorateTextWith Superscript x = sup ! class_ "idocSuperscript" $ x
      decorateTextWith Subscript x = sub ! class_ "idocSubscript" $ x
      decorateTextWith Quoted x = q ! class_ "idocQuoted" $ x

instance BlockMarkup a => ToMarkup (Doc a) where
  toMarkup d = article ! class_ "idocDoc" $
               (toMarkup $ d^.docTitle) ++
               (concatMap toMarkup $ d^.docSections)

instance BlockMarkup a => ToMarkup (Core a) where
  toMarkup (SC sc) = toMarkup sc
  toMarkup (CC cc) = toMarkup cc

instance ToMarkup SimpleCore where
  toMarkup (TextC t) = toMarkup t
  toMarkup (QTextC qt) = toMarkup qt
  toMarkup (LinkC l) = toMarkup l
  toMarkup (InlineMathC im) = toMarkup im
  toMarkup (MarkupC m) = toMarkup m

instance ToMarkup Paragraph where
  toMarkup pa = p ! class_ "idocParagraph" $
                concatMap toMarkup $ pa^.paraContents

instance BlockMarkup a => ToMarkup (ComplexCore a) where
  toMarkup (ListC l) = toMarkup l
  toMarkup (BlockC b_) = toMarkup b_
  toMarkup (ParagraphC p_) = toMarkup p_

instance BlockMarkup a => ToMarkup (Block a) where
  toMarkup b_ = blockMarkup (b_^.bAttrs) (b_^.bTitle) (b_^.bSetID) (b_^.bType)

instance (RecApplicative xs, AllAllSat '[BlockMarkup] xs) => BlockMarkup (CoRec Data.Vinyl.Functor.Identity xs) where
  blockMarkup a_ t s x = getIdentity $ onCoRec (Proxy :: Proxy '[BlockMarkup]) (blockMarkup a_ t s) x

instance ToMarkup DocTitle where
  toMarkup (DocTitle dt_) = h1 ! class_ "idocDocTitle" $
                            concatMap toMarkup dt_
-- * Some Helper Functions

-- | FIXME: This is truly fucked.
idHelper :: (Text -> t) -> ID -> t
idHelper decorator id_ = let (base_, hash_) =
                               case (id_^.idProtocol, id_^.idHash) of
                                 (Just (Protocol "youtube"), Nothing) -> 
                                   ("https://youtube.com/embed/", "")
                                 (Just (Protocol "youtube"), Just _) -> 
                                   error "got youtube protocol with a hash!?"
                                 (Nothing, Nothing) -> 
                                   ("http://www.independentlearning.science/tiki/", "")
                                 (Just (Protocol p_), Just (IDHash h)) ->
                                   (p_ ++ "://", h)
                                 (Nothing, Just (IDHash h)) -> ("/", h)
                                 (Just (Protocol p_), Nothing) -> (p_ ++ "://", "")
                         in
                           decorator $ base_ ++
                           (concatMap (\(IDBase x) -> x) $ intersperse (IDBase "/") (id_^.idBase)) ++
                           hash_

vectorBlockToMarkup :: B.ToMarkup a => 
                       B.AttributeValue 
                    -> (B.Html -> B.Html)
                    -> Vector a 
                    -> B.Html
vectorBlockToMarkup cls dec vb = B.div B.! A.class_ cls $
                                 dec $
                                 concatMap B.toMarkup
                                 vb

verbatimBlockToMarkup :: B.AttributeValue 
                      -> (B.Html -> B.Html)
                      -> Vector Token
                      -> B.Html
verbatimBlockToMarkup cls dec vb = B.div B.! A.class_ cls $
                                   dec $
                                   concatMap (\x -> if x == Newline then
                                                      B.toMarkup B.br
                                                    else
                                                      B.toMarkup x)
                                   vb

newtype LinkLevel = LinkLevel Int deriving (Eq, Ord, Show)

listLinks :: Doc a -> Vector (SetID, LinkLevel)
listLinks (Doc { _docSections = ss
               , _docSetID = dsid }) =
  catMaybes (singleton ((\x -> (x, LinkLevel 1)) <$> dsid)) <>
  concatMap (\(Section { _secSetID = ssid
                       , _secContents = scnts }) ->
                catMaybes $ ((\case
                                CC (BlockC (Block { _bSetID = bsid })) -> do
                                  sid <- bsid
                                  return (sid, LinkLevel 3)
                                CC (ParagraphC (Paragraph { _paraSetID = psid })) -> do
                                  sid <- psid
                                  return (sid, LinkLevel 3)
                                _ -> Nothing
                                -- FIXME: Add lists here
                            ) <$> scnts) <> singleton ((\x -> (x, LinkLevel 2)) <$> ssid)) ss

mID :: Maybe SetID -> (Html -> Html)
mID mid = case mid of
  Nothing -> CP.id
  Just id_ -> (\x -> x ! A.id (toValue id_))

