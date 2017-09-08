{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | Syntax.hs
-- Author: Matt Walker
-- License: https://opensource.org/licenses/BSD-2-Clause
-- Created: Aug 24, 2017
-- Summary: 

module Text.IDoc.Syntax where

import ClassyPrelude as CP

import Data.Data

import qualified Text.Megaparsec.Prim as Prim
import Text.Megaparsec.Pos

import Control.Lens

type Row = Word
type Col = Word

data DebugInfo = DebugInfo { _diStart :: !(Row, Col)
                           , _diEnd :: !(Row, Col)
                           }
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

data DebugToken d = DebugToken { _dtInfo :: d
                               , _dtToken :: Token
                               }
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

type DToken = DebugToken DebugInfo

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

newtype IDocTokenStream = IDocTokenStream { unStream :: Vector DToken }

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
          
data Core = SC SimpleCore
          | CC ComplexCore
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

data SimpleCore =
    TextC Text
  | QTextC QText
  | LinkC Link
  | InlineMathC InlineMath
  | MarkupC Markup
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

data ComplexCore =
    ListC List
  | BlockC Block
  | ParagraphC Paragraph
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

data Paragraph = Paragraph { _paraContents :: Vector SimpleCore
                           , _paraSetID :: Maybe SetID
                           }
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

newtype DocTitle = DocTitle { unDocTitle :: Vector SimpleCore }
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

data Doc = Doc { _docTitle :: DocTitle
               , _docSections :: Vector Section }
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

data TextType = Strong
              | Emphasis 
              | Monospace
              | Superscript
              | Subscript
              | Quoted
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

data QText = QText { _qtText :: Vector SimpleCore
                   , _qtType :: TextType
                   }
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

data SetID = SetID { _sidName :: IDHash }
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

newtype IDHash = IDHash Text deriving (Eq, Ord, Show, Data, Typeable, Generic)

data AttrMap = AttrMap { _amMap :: Map AttrName (Maybe AttrValue) }
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

newtype AttrName = AttrName Text deriving (Eq, Ord, Show, Data, Typeable, Generic)
newtype AttrValue = AttrValue Text deriving (Eq, Ord, Show, Data, Typeable, Generic)

data LinkType = Internal 
              | Back
              | Out
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

newtype LinkText = LinkText { unLinkText :: Vector SimpleCore }
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

data Link = Link { _linkText :: LinkText
                 , _linkAttrs :: AttrMap
                 , _linkLocation :: ID
                 , _linkType :: LinkType
                 }
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

data ID = ID { _idProtocol :: Maybe Protocol
             , _idBase :: Vector IDBase
             , _idHash :: Maybe IDHash
             }
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

newtype Protocol = Protocol Text deriving (Eq, Ord, Show, Data, Typeable, Generic)
newtype IDBase = IDBase Text deriving (Eq, Ord, Show, Data, Typeable, Generic)

data ListType = Unordered
              | Ordered
              | Labelled
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

data List = List { _listContents :: Vector ListItem }
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

newtype ListLabel = ListLabel { unListLabel :: Vector SimpleCore }
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

data ListItem = ListItem { _liAttrs :: AttrMap
                         , _liLabel :: Maybe ListLabel
                         , _liContents :: Vector SimpleCore
                         , _liSetID :: Maybe SetID
                         , _liType :: ListType
                         }
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

data MarkupType = Footnote
                | FootnoteRef
                | Citation
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

data Markup = Markup { _muType :: MarkupType
                     , _muAttrs :: AttrMap
                     , _muContents :: Vector SimpleCore
                     , _muSetID :: Maybe SetID
                     }
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

data InlineMath = InlineMath { _imAttrs :: AttrMap
                             , _imContents :: Vector Token
                             , _imSetID :: Maybe SetID
                             }
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

data BlockType = PrerexB { _prerex :: Prerex }
               | IntroductionB { _introduction :: Introduction }
               | MathB { _math :: Math }
               | EquationB { _equation :: Equation }
               | EqnArrayB { _eqnArray :: EqnArray }
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

newtype BlockTitle = BlockTitle { unBlockTitle :: Vector SimpleCore }
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

data Block = Block { _bType :: BlockType
                   , _bAttrs :: AttrMap
                   , _bTitle :: Maybe BlockTitle
                   , _bSetID :: Maybe SetID
                   }
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

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

data EqnArray = EqnArray { _eqnArrayContents :: Vector Token }
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
makeLenses ''EqnArray
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
