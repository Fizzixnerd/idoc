{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
import qualified Data.List.NonEmpty as NE

import Data.Vinyl
import Data.Vinyl.CoRec
import Data.Vinyl.Functor

import qualified Data.Vector as V

import Text.Blaze.Html5 as B
import Text.Blaze.Html5.Attributes as A

import Text.LaTeX as L hiding ((<>))
import Text.LaTeX.Base.Class
import Text.LaTeX.Packages.AMSMath as M hiding (to)
import Text.LaTeX.Packages.Hyperref as H

import Text.IDoc.Render.Tex

import qualified Text.Megaparsec as MP
import Text.Megaparsec.Stream
import Text.Megaparsec.Pos

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
  deriving (Eq, Show)

-- | Megaparsec Stream instance so that this properly works with the
-- rest of the library.
instance Stream IDocTokenStream where
  type Token IDocTokenStream = DToken
  type Tokens IDocTokenStream = Vector DToken
  tokensToChunk _ = fromList
  chunkToTokens _ = toList
  chunkLength _ = length
  chunkEmpty _ = null
  take1_ (IDocTokenStream ts) = if V.null ts then
                                  Nothing
                                else
                                  Just (V.head ts, IDocTokenStream $ V.tail ts)
  takeN_ n its | n <= 0 = Just (V.empty, its)
  takeN_ _ (IDocTokenStream ts) | V.null ts = Nothing
  takeN_ n (IDocTokenStream ts) = Just $ IDocTokenStream <$> (V.splitAt n ts)
  takeWhile_ p_ (IDocTokenStream ts) = IDocTokenStream <$> (V.span p_ ts)
  showTokens _ (x NE.:| xs) = show $ (\DebugToken {..} -> unToken _dtToken) <$> (x : xs)
  reachOffset newOffset pstate =
    let oldOffset = MP.pstateOffset pstate
        oldSourcePos = MP.pstateSourcePos pstate
        oldStream = MP.pstateInput pstate
        getNewStream offset stream =
          let mNewStreamTuple = takeN_ offset stream
          in
            maybe (V.empty, stream) CP.id mNewStreamTuple
        getNewSourcePos stream old =
          let mNextToken = fst <$> take1_ stream
          in
            maybe old (\dtoken ->
                          old
                          { sourceLine = mkPos $ fst $ _diStart $ _dtInfo dtoken
                          , sourceColumn = mkPos $ snd $ _diStart $ _dtInfo dtoken
                          }) mNextToken
        (jumpedContent, newStream) = getNewStream (newOffset - oldOffset) oldStream
        newlineIndices = V.findIndices (\dtoken ->
                                          _dtToken dtoken == Newline) jumpedContent
        lastIndex = if V.null newlineIndices
                    then Nothing
                    else Just $ V.last newlineIndices
        prefix = maybe jumpedContent (\idx -> snd $ V.splitAt idx jumpedContent) lastIndex
        restOfLine = fst $ takeWhile_ (\dtoken -> _dtToken dtoken /= Newline) newStream
        wholeLine = prefix ++ restOfLine
        printedLine = if V.null wholeLine
                      then "<empty line>"
                      else concatMap (unToken . _dtToken) wholeLine
        newSourcePos = getNewSourcePos oldStream oldSourcePos
        newPosState = pstate
                      { MP.pstateInput = newStream
                      , MP.pstateOffset = newOffset
                      , MP.pstateSourcePos = newSourcePos
                      }
    in
      (newSourcePos, unpack printedLine, newPosState)

-- | One of a `SimpleCore' or a `ComplexCore'; holds most constructs in the
-- language.
data Core m b = SC (SimpleCore m)
              | CC (ComplexCore m b)
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

-- | Sum type for holding `Text', `QText' ("quoted text"), `Link's, `InlineMath'
-- or `Markup'. Used inside `Paragraph's and titles (like `Section' headings and
-- so on).
data SimpleCore m =
    TextC Text
  | QTextC (QText m)
  | LinkC (Link m)
  | InlineMathC (InlineMath m)
  | MarkupC (Text.IDoc.Syntax.Markup m)
  | CommentC (Vector Text.IDoc.Syntax.Token)
  deriving (Eq, Ord, Show, Data, Typeable, Generic, Functor)

-- | Sum type for holding the major organizing constructs of the
-- language: `List's, `Block's and `Paragraph's.
data ComplexCore m b =
    BlockC (Block m b)
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

-- | A single parsed idoc document.
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
newtype LinkText m = LinkText { _unLinkText :: Text }
  deriving (Eq, Ord, Show, Data, Typeable, Generic, Functor)

-- | A Link around (or out of) a `Doc'.  See `LinkType' for the types
-- of possible links.  See `ID' for the format of links.
data Link m = Link { _linkText :: Maybe (LinkText m)
                   , _linkAttrs :: AttrMap
                   , _linkLocation :: ID
                   , _linkType :: LinkType
                   }
  deriving (Eq, Ord, Show, Data, Typeable, Generic, Functor)

newtype LinkConstraint = LinkConstraint { unLinkConstraint :: Vector IDBase }
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

data LinkConstraints = LinkAny
                     | LinkNone
                     | LinkConstraints (Set LinkConstraint)
                     deriving (Eq, Ord, Show, Data, Typeable, Generic)

instance Semigroup LinkConstraints where
  LinkAny <> _ = LinkAny
  _ <> LinkAny = LinkAny
  x <> LinkNone = x
  LinkNone <> x = x
  LinkConstraints x <> LinkConstraints y = LinkConstraints (x `union` y)

instance Monoid LinkConstraints where
  mempty = LinkNone

data Constraints = Constraints
  { _lcBackConstraints :: LinkConstraints
  , _lcOutConstraints :: LinkConstraints
  } deriving (Eq, Ord, Show, Data, Typeable, Generic)

data BadLink m b = BadLink
  { _blLink :: Link m
  , _blLocation :: Maybe (Core m b)
  } deriving (Eq, Ord, Show, Data, Typeable, Generic)

linkPretty :: Link m -> Text
linkPretty link_ =
  let prettyIDBase = concat $ intersperse "/" $ _unIDBase <$> (_idBase $  _linkLocation link_)
  in
    prettyIDBase

badLinkPretty :: BadLink m b -> Text
badLinkPretty badLink = "Bad link to: " <> (linkPretty $ _blLink badLink) <> ".\n"

class CheckLinks m b a where
  checkLinks :: Constraints -> Maybe (Core m b) -> a -> Vector (BadLink m b)

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
  toMarkup (CommentC _) = CP.mempty

instance MarkupMarkup m => ToMarkup (Paragraph m) where
  toMarkup p_ = p ! class_ "idocParagraph" $
                concatMap toMarkup $ p_^.paraContents

instance (MarkupMarkup m, BlockMarkup m (b m)) => ToMarkup (ComplexCore m b) where
  toMarkup (BlockC b_) = toMarkup b_
  toMarkup (ParagraphC p_) = toMarkup p_

instance (MarkupMarkup m, BlockMarkup m (b m)) => ToMarkup (Block m b) where
  toMarkup b_ = blockMarkup (b_^.bAttrs) (b_^.bTitle) (b_^.bSetID) (b_^.bType)

instance MarkupMarkup m => ToMarkup (DocTitle m) where
  toMarkup (DocTitle dt_) = h1 ! class_ "idocDocTitle" $
                            concatMap toMarkup dt_

instance ( MarkupMarkup m
         , RPureConstrained (BlockMarkup m) xs) =>
  BlockMarkup m (CoRec Data.Vinyl.Functor.Identity xs) where
  blockMarkup a_ t s x = getIdentity $ onCoRec @(BlockMarkup m) (fmap $ blockMarkup a_ t s) x

instance RPureConstrained MarkupMarkup xs =>
  MarkupMarkup (CoRec Data.Vinyl.Functor.Identity xs) where
  markupMarkup a_ s x = getIdentity $ onCoRec @MarkupMarkup (fmap $ markupMarkup a_ s) x

-- * Some Helper Functions
vectorBlockToMarkup :: B.ToMarkup a =>
                       B.AttributeValue -- ^ Html class
                    -> (B.Html -> B.Html) -- ^ decorator
                    -> Vector a -- ^ vector block
                    -> B.Html
vectorBlockToMarkup cls dec vb = B.div B.! A.class_ cls $
                                 dec $
                                 concatMap B.toMarkup vb

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

-- newtype LinkLevel = LinkLevel Int
--   deriving (Eq, Ord, Show)

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

-- listLinks :: Doc m b -> Protocol -> Vector IDBase -> Vector (Link m, LinkLevel)
-- listLinks d proto rel_ = singleton (docLink, LinkLevel 1) <> sectionLinks
--   where
--     docLink = Link { _linkText = Just $ LinkText $ d^.docTitle.unDocTitle
--                    , _linkAttrs = AttrMap CP.mempty
--                    , _linkLocation = ID { _idProtocol = Just proto
--                                         , _idBase = rel_
--                                         , _idHash = Just $ IDHash "" }
--                    , _linkType = Internal
--                    }
--     sectionLinks = (\s -> ( Link { _linkText = Just $ LinkText $ s^.secTitle.unSectionTitle
--                                  , _linkAttrs = AttrMap CP.mempty
--                                  , _linkLocation = ID { _idProtocol = Just proto
--                                                       , _idBase = rel_
--                                                       , _idHash = _sidName <$> (s^.secSetID)
--                                                       }
--                                  , _linkType = Internal
--                                  }
--                           , LinkLevel 2)) <$> (d^.docSections)

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
  texy (CommentC _) = CP.mempty

instance (Markupy m, Blocky m (b m)) => Texy (ComplexCore m b) where
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
  texy (LinkText lt) = texy lt

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

instance Blocky m (b m) => Texy (Block m b) where
  texy b_ = blocky (b_^.bAttrs) (b_^.bTitle) (b_^.bSetID) (b_^.bType)

instance (CheckLinks m b (b m), CheckLinks m b m) => CheckLinks m b (Doc m b) where
  checkLinks constraints container doc =
    checkLinks constraints container (doc^.docTitle)
    ++ concatMap (checkLinks constraints container) (doc^.docSections)

instance CheckLinks m b m => CheckLinks m b (SectionTitle m) where
  checkLinks constraints container (SectionTitle st) =
    concatMap (checkLinks constraints container) st

instance (CheckLinks m b (b m), CheckLinks m b m) => CheckLinks m b (Section m b) where
  checkLinks constraints container sec_ =
    (checkLinks constraints container (sec_^.secTitle))
    ++ (concatMap (checkLinks constraints container) (sec_^.secContents))

instance CheckLinks m b m => CheckLinks m b (DocTitle m) where
  checkLinks constraints container (DocTitle dtc) =
    concatMap (checkLinks constraints container) dtc

instance (CheckLinks m b (b m), CheckLinks m b m) => CheckLinks m b (Core m b) where
  checkLinks constraints _ c@(SC sc) = checkLinks constraints (Just c) sc
  checkLinks constraints _ c@(CC cc) = checkLinks constraints (Just c) cc

instance (CheckLinks m b (b m), CheckLinks m b m) => CheckLinks m b (ComplexCore m b) where
  checkLinks constraints container (BlockC b_) = checkLinks constraints container b_
  checkLinks constraints container (ParagraphC p_) = checkLinks constraints container p_

instance CheckLinks m b m => CheckLinks m b (Text.IDoc.Syntax.Markup m) where
  checkLinks constraints container mu_ = checkLinks constraints container (mu_^.muType)

instance CheckLinks m b m => CheckLinks m b (Paragraph m) where
  checkLinks constraints container p_ =
    concatMap (checkLinks constraints container) (p_^.paraContents)

instance (CheckLinks m b (b m), CheckLinks m b m) => CheckLinks m b (Block m b) where
  checkLinks constraints container b_ =
    (maybe mempty (checkLinks constraints container) (b_^.bTitle))
    ++ (checkLinks constraints container (b_^.bType))

instance CheckLinks m b (Link m) where
  checkLinks _ _ (Link { _linkType = Internal }) = mempty
  checkLinks (Constraints { _lcBackConstraints = LinkAny }) _ (Link { _linkType = Back _}) = mempty
  checkLinks (Constraints { _lcBackConstraints = LinkNone }) container l@(Link { _linkType = Back _ }) =
    singleton BadLink
    { _blLink = l
    , _blLocation = container
    }
  checkLinks (Constraints { _lcBackConstraints = LinkConstraints constraints }) container l@(Link { _linkType = Back _
                                                                                                  , _linkLocation = location }) =
    let ID { _idBase = idBase_ } = location
    in
      if not $ LinkConstraint idBase_ `member` constraints
      then singleton BadLink
           { _blLink = l
           , _blLocation = container
           }
      else mempty
  checkLinks (Constraints { _lcOutConstraints = LinkAny }) _ (Link { _linkType = Out _ }) = mempty
  checkLinks (Constraints { _lcOutConstraints = LinkNone }) container l@(Link { _linkType = Out _ }) =
    singleton BadLink
    { _blLink = l
    , _blLocation = container
    }
  checkLinks (Constraints { _lcOutConstraints = LinkConstraints constraints }) container l@(Link { _linkType = Out _
                                                                                                 , _linkLocation = location }) =
    let ID { _idBase = idBase_ } = location
    in
      if not $ LinkConstraint idBase_ `member` constraints
      then singleton BadLink
           { _blLink = l
           , _blLocation = container
           }
      else mempty

instance CheckLinks m b m => CheckLinks m b (SimpleCore m) where
  checkLinks constraints container (LinkC l) = checkLinks constraints container l
  checkLinks constraints container (MarkupC m) = checkLinks constraints container m
  checkLinks _ _ _ = mempty

instance CheckLinks m b m => CheckLinks m b (BlockTitle m) where
  checkLinks constraints container (BlockTitle btc) =
    concatMap (checkLinks constraints container) btc

-- Type-level magic!
instance ( Markupy m
         , RPureConstrained (Blocky m) xs) =>
  Blocky m (CoRec Data.Vinyl.Functor.Identity xs) where
  blocky a_ t s x = getIdentity $ onCoRec @(Blocky m) (fmap $ blocky a_ t s) x

instance RPureConstrained Markupy xs =>
  Markupy (CoRec Data.Vinyl.Functor.Identity xs) where
  markupy a_ s x = getIdentity $ onCoRec @Markupy (fmap $ markupy a_ s) x

instance RPureConstrained (CheckLinks m b) xs =>
  CheckLinks m b (CoRec Data.Vinyl.Functor.Identity xs) where
  checkLinks constraints container x = getIdentity $ onCoRec @(CheckLinks m b) (fmap $ checkLinks constraints container) x
