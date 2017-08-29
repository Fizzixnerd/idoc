{-# LANGUAGE TemplateHaskell #-}
-- | Syntax.hs
-- Author: Matt Walker
-- License: https://opensource.org/licenses/BSD-2-Clause
-- Created: Aug 24, 2017
-- Summary: 

{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- {-# LANGUAGE TemplateHaskell #-}
-- {-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Text.IDoc.Syntax where

import Control.Lens

import ClassyPrelude

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
  deriving (Eq, Ord, Show)

data Core = SC SimpleCore
          | CC ComplexCore
  deriving (Eq, Ord, Show)

data SimpleCore =
    TextC Text
  | QTextC QText
  | LinkC Link
  | InlineMathC InlineMath
  | MarkupC Markup
  deriving (Eq, Ord, Show)

data ComplexCore =
    ListC List
  | BlockC Block
  | ParagraphC Paragraph
  deriving (Eq, Ord, Show)

data Paragraph = Paragraph { _paraContents :: Vector SimpleCore
                           , _paraSetID :: Maybe SetID
                           }
  deriving (Eq, Ord, Show)

data Doc = Doc { _docTitle :: Vector SimpleCore
               , _docSections :: Vector Section }
  deriving (Eq, Ord, Show)

data TextType = Strong
              | Emphasis 
              | Monospace
              | Superscript
              | Subscript
  deriving (Eq, Ord, Show)

data QText = QText { _qtText :: Vector SimpleCore
                   , _qtType :: TextType
                   }
  deriving (Eq, Ord, Show)

data SetID = SetID { _sidName :: IDHash }
  deriving (Eq, Ord, Show)

newtype IDHash = IDHash Text deriving (Eq, Ord, Show)

data AttrMap = AttrMap { _amMap :: Map AttrName (Maybe AttrValue) }
  deriving (Eq, Ord, Show)

newtype AttrName = AttrName Text deriving (Eq, Ord, Show)
newtype AttrValue = AttrValue Text deriving (Eq, Ord, Show)

data LinkType = Internal 
              | Back
              | Out
  deriving (Eq, Ord, Show)

data Link = Link { _linkText :: Vector SimpleCore
                 , _linkAttrs :: AttrMap
                 , _linkLocation :: ID
                 , _linkType :: LinkType
                 }
  deriving (Eq, Ord, Show)

data ID = ID { _idProtocol :: Maybe Protocol
             , _idBase :: Vector IDBase
             , _idHash :: Maybe IDHash
             }
  deriving (Eq, Ord, Show)

newtype Protocol = Protocol Text deriving (Eq, Ord, Show)
newtype IDBase = IDBase Text deriving (Eq, Ord, Show)

data ListType = Unordered
              | Ordered
              | Labelled
  deriving (Eq, Ord, Show)

data List = List { _listContents :: Vector ListItem }
  deriving (Eq, Ord, Show)

data ListItem = ListItem { _liAttrs :: AttrMap
                         , _liLabel :: Maybe (Vector SimpleCore)
                         , _liContents :: Vector SimpleCore
                         , _liSetID :: Maybe SetID
                         , _liType :: ListType
                         }
  deriving (Eq, Ord, Show)

data MarkupType = Footnote
                | FootnoteRef
                | Citation
  deriving (Eq, Ord, Show)

data Markup = Markup { _muType :: MarkupType
                     , _muAttrs :: AttrMap
                     , _muContents :: Vector SimpleCore
                     , _muSetID :: Maybe SetID
                     }
  deriving (Eq, Ord, Show)

data InlineMath = InlineMath { _imAttrs :: AttrMap
                             , _imContents :: Vector Token
                             , _imSetID :: Maybe SetID
                             }
  deriving (Eq, Ord, Show)

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
               | AdmonitionB { _admonition :: Admonition }
               | SideNoteB { _sidenote :: SideNote }
               | ExampleB { _example :: Example }
               | ExerciseB { _exercise :: Exercise }
               | BibliographyB { _bibliography :: Bibliography }
               | FurtherReadingB { _furtherReading :: FurtherReading }
               | SummaryB { _summary :: Summary }
               | RecallB { _recall :: Recall }
  deriving (Eq, Ord, Show)

data Block = Block { _bType :: BlockType
                   , _bAttrs :: AttrMap
                   , _bTitle :: Maybe (Vector SimpleCore)
                   , _bSetID :: Maybe SetID
                   }
  deriving (Eq, Ord, Show)

data Prerex = Prerex { _prerexContents :: Vector PrerexItem }
  deriving (Eq, Ord, Show)

data PrerexItem = PrerexItem { _prerexItemPath :: Vector Text
                             , _prerexItemDescription :: Maybe (Vector SimpleCore)
                             }
  deriving (Eq, Ord, Show)

data Introduction = Introduction { _introductionContents :: Vector Core }
  deriving (Eq, Ord, Show)

data Math = Math { _mathContents :: Vector Token }
  deriving (Eq, Ord, Show)

data Equation = Equation { _equationContents :: Vector Token }
  deriving (Eq, Ord, Show)

data EqnArray = EqnArray { _eqnArrayContents :: Vector Token }
  deriving (Eq, Ord, Show)

data Theorem = Theorem { _theoremContents :: Vector Core }
  deriving (Eq, Ord, Show)

data Lemma = Lemma { _lemmaContents :: Vector Core }
  deriving (Eq, Ord, Show)

data Corollary = Corollary { _corollaryContents :: Vector Core }
  deriving (Eq, Ord, Show)

data Proposition = Proposition { _propositionContents :: Vector Core }
  deriving (Eq, Ord, Show)

data Conjecture = Conjecture { _conjectureContents :: Vector Core }
  deriving (Eq, Ord, Show)

data Axiom = Axiom { _axiomContents :: Vector Core }
  deriving (Eq, Ord, Show)

data Proof = Proof { _proofContents :: Vector Core }
  deriving (Eq, Ord, Show)

data Quote = Quote { _quoteContents :: Vector SimpleCore }
  deriving (Eq, Ord, Show)

data Code = Code { _codeContents :: Vector Token }
  deriving (Eq, Ord, Show)

data Image = Image { _imageContents :: Link }
  deriving (Eq, Ord, Show)

data Video = Video { _videoContents :: Link }
  deriving (Eq, Ord, Show)

data YouTube = YouTube { _youTubeContents :: Link }
  deriving (Eq, Ord, Show)

data Connection = Connection { _connectionContents :: Vector Core }
  deriving (Eq, Ord, Show)

data Definition = Definition { _definitionContents :: Vector Core }
  deriving (Eq, Ord, Show)

data Intuition = Intuition { _intuitionContents :: Vector Core }
  deriving (Eq, Ord, Show)

data Admonition = Admonition { _admonitionContents :: Vector Core }
  deriving (Eq, Ord, Show)

data SideNote = SideNote { _sideNoteContents :: Vector Core }
  deriving (Eq, Ord, Show)

data Example = Example { _exampleContents :: Vector Core }
  deriving (Eq, Ord, Show)

data Exercise = Exercise { _exerciseContents :: Vector Core }
  deriving (Eq, Ord, Show)

data Bibliography = Bibliography { _bibliographyContents :: Vector BibItem }
  deriving (Eq, Ord, Show)

data BibItem = BibItem { _biAuthor :: Text
                       , _biTitle :: Text
                       , _biYear :: Text
                       }
  deriving (Eq, Ord, Show)

data FurtherReading = FurtherReading { _furtherReadingContents :: Vector Core }
  deriving (Eq, Ord, Show)

data Summary = Summary { _summaryContents :: Vector Core }
  deriving (Eq, Ord, Show)

data Recall = Recall { _recallContents :: Vector Core }
  deriving (Eq, Ord, Show)

data SectionType = Preamble
                 | TopSection
                 | SubSection
  deriving (Eq, Ord, Show)

data Section = Section { _secType :: SectionType
                       , _secAttrs :: AttrMap
                       , _secContents :: Vector Core
                       , _secTitle :: Vector SimpleCore
                       , _secSetID :: Maybe SetID
                       }
  deriving (Eq, Ord, Show)

makeLenses ''AttrMap
makeLenses ''QText
makeLenses ''SetID
makeLenses ''Section
makeLenses ''Link
makeLenses ''List
makeLenses ''Markup
makeLenses ''InlineMath
makeLenses ''BlockType
makeLenses ''Block
makeLenses ''Doc

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
makeLenses ''Admonition
makeLenses ''SideNote
makeLenses ''Example
makeLenses ''Exercise
makeLenses ''Bibliography
makeLenses ''FurtherReading
makeLenses ''Summary
makeLenses ''Recall
