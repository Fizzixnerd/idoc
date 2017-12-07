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

import ClassyPrelude as CP

import Data.Data

import qualified Text.Megaparsec.Prim as Prim
import Text.Megaparsec.Pos

import Control.Lens

-- * Syntax
--
-- Everything in this file is defined Data, Typeable, and Generic, as
-- well as the usual Eq, Show, Ord.

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
data Core = SC SimpleCore
          | CC ComplexCore
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

-- | Sum type for holding `Text', `QText' ("quoted text"), `Link's,
-- `InlineMath' or `Markup'.  Used inside `Paragraph's and titles (like
-- `Section' headings and so on).
data SimpleCore =
    TextC Text
  | QTextC QText
  | LinkC Link
  | InlineMathC InlineMath
  | MarkupC Markup
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

-- | Sum type for holding the major organizing constructs of the
-- language: `List's, `Block's and `Paragraph's.
data ComplexCore =
    ListC List
  | BlockC Block
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
data Doc = Doc { _docTitle :: DocTitle
               , _docSections :: Vector Section 
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

-- | The "hash" bit of an `ID'.  It's the part that comes after the
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
data InlineMath = InlineMath { _imAttrs :: AttrMap
                             , _imContents :: Vector Token
                             , _imSetID :: Maybe SetID
                             }
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

-- | Sum type for the various types of blocks allowed in idoc.
data BlockType = PrerexB { _prerex :: Prerex }
               | IntroductionB { _introduction :: Introduction }
               | MathB { _math :: Math }
               | EquationB { _equation :: Equation }
               | AlignB { _align :: Align }
               | TheoremB { _theorem :: Theorem }
               | LemmaB { _lemma :: Lemma }
               | CorollaryB { _corollary :: Corollary }
               | PropositionB { _proposition :: Proposition }
               | ConjectureB { _conjecture :: Conjecture }
               | AxiomB { _axiom :: Axiom }
               | ProofB { _proof :: Proof }
               | QuoteB { _quote :: Quote }
               | CodeB { _code :: Code }
               | ImageB { _image :: Image }
               | VideoB { _video :: Video }
               | YouTubeB { _youTube :: YouTube }
               | ConnectionB { _connection :: Connection }
               | DefinitionB { _definition :: Definition }
               | IntuitionB { _intuition :: Intuition }
               | InfoB { _info :: Info }
               | TipB { _tip :: Tip }
               | CautionB { _caution :: Caution }
               | WarningB { _warning :: Warning }
               | SideNoteB { _sidenote :: SideNote }
               | ExampleB { _example :: Example }
               | ExerciseB { _exercise :: Exercise }
               | BibliographyB { _bibliography :: Bibliography }
               | FurtherReadingB { _furtherReading :: FurtherReading }
               | SummaryB { _summary :: Summary }
               | RecallB { _recall :: Recall }
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

-- | Newtype for titles of `Block's.
newtype BlockTitle = BlockTitle { unBlockTitle :: Vector SimpleCore }
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

-- | Common wrapper for blocks.  Most of the interesting parts will be
-- found inside `_bType'.  See `BlockType'.
data Block = Block { _bType :: BlockType
                   , _bAttrs :: AttrMap
                   , _bTitle :: Maybe BlockTitle
                   , _bSetID :: Maybe SetID
                   }
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

-- | The prerequisites
data Prerex = Prerex { _prerexContents :: Vector PrerexItem }
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

data PrerexItem = PrerexItem { _prerexItemPath :: ID
                             , _prerexItemDescription :: Vector SimpleCore
                             }
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

data Introduction = Introduction { _introductionContents :: Vector Core }
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

data Math = Math { _mathContents :: Vector Token }
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

data Equation = Equation { _equationContents :: Vector Token }
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

data Align = Align { _alignContents :: Vector Token }
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

data Theorem = Theorem { _theoremContents :: (Vector Core, Maybe (Vector Core)) }
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

data Lemma = Lemma { _lemmaContents :: (Vector Core, Maybe (Vector Core)) }
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

data Corollary = Corollary { _corollaryContents :: (Vector Core, Maybe (Vector Core)) }
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

data Proposition = Proposition { _propositionContents :: (Vector Core, Maybe (Vector Core)) }
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

data Conjecture = Conjecture { _conjectureContents :: Vector Core }
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

data Axiom = Axiom { _axiomContents :: Vector Core }
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

data Proof = Proof { _proofContents :: Vector Core }
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

data Quote = Quote { _quoteContents :: Vector SimpleCore }
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

data Code = Code { _codeContents :: Vector Token }
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

data Image = Image { _imageContents :: (Link, Maybe (Vector SimpleCore)) }
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

data Video = Video { _videoContents :: (Link, Maybe (Vector SimpleCore)) }
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

data YouTube = YouTube { _youTubeContents :: (Link, Maybe (Vector SimpleCore )) }
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

data Connection = Connection { _connectionContents :: Vector Core }
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

data Definition = Definition { _definitionContents :: Vector Core }
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

data Intuition = Intuition { _intuitionContents :: Vector Core }
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

data Info = Info { _infoContents :: Vector Core }
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

data Tip = Tip { _tipContents :: Vector Core }
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

data Caution = Caution { _cautionContents :: Vector Core }
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

data Warning = Warning { _warningContents :: Vector Core }
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

data SideNote = SideNote { _sideNoteContents :: Vector Core }
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

data Example = Example { _exampleContents :: (Vector Core, Vector Core) }
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

data Exercise = Exercise { _exerciseContents :: Vector Core }
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

data Bibliography = Bibliography { _bibliographyContents :: Vector BibItem }
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

data BibItem = BibItem { _biAuthor :: Text
                       , _biTitle :: Text
                       , _biYear :: Text
                       }
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

data FurtherReading = FurtherReading { _furtherReadingContents :: Vector Core }
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

data Summary = Summary { _summaryContents :: Vector Core }
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

data Recall = Recall { _recallContents :: (Vector Link, Vector Core) }
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

data SectionType = Preamble
                 | TopSection
                 | SubSection
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

newtype SectionTitle = SectionTitle { unSectionTitle :: Vector SimpleCore }
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

data Section = Section { _secType :: SectionType
                       , _secAttrs :: AttrMap
                       , _secContents :: Vector Core
                       , _secTitle :: SectionTitle
                       , _secSetID :: Maybe SetID
                       }
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

-- * Lenses
--
-- We then define lenses for nearly everything in the module.

makeLenses ''DebugInfo
makeLenses ''DebugToken
makeLenses ''AttrMap
makeLenses ''QText
makeLenses ''SetID
makeLenses ''Section
makeLenses ''Link
makeLenses ''List
makeLenses ''ListItem
makeLenses ''Markup
makeLenses ''InlineMath
makeLenses ''BlockType
makeLenses ''Block
makeLenses ''Doc
makeLenses ''Paragraph
makeLenses ''ID
makeLenses ''BibItem

makeLenses ''Prerex
makeLenses ''PrerexItem
makeLenses ''Introduction
makeLenses ''Math
makeLenses ''Equation
makeLenses ''Align
makeLenses ''Theorem
makeLenses ''Lemma
makeLenses ''Corollary
makeLenses ''Proposition
makeLenses ''Conjecture
makeLenses ''Axiom
makeLenses ''Proof
makeLenses ''Quote
makeLenses ''Code
makeLenses ''Image
makeLenses ''Video
makeLenses ''YouTube
makeLenses ''Connection
makeLenses ''Definition
makeLenses ''Intuition
makeLenses ''Info
makeLenses ''Tip
makeLenses ''Caution
makeLenses ''Warning
makeLenses ''SideNote
makeLenses ''Example
makeLenses ''Exercise
makeLenses ''Bibliography
makeLenses ''FurtherReading
makeLenses ''Summary
makeLenses ''Recall
