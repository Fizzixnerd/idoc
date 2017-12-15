module Text.IDoc.Blocks.IntroOutro where

import Text.IDoc.Syntax
import Text.IDoc.Parse
import Text.IDoc.Render.Html5.Card

import Text.Blaze.Html5

import Data.Data

import Control.Lens

import ClassyPrelude

data IntroOutroB a = IntroductionB { _introduction :: Introduction a }
                   | SummaryB { _summary :: Summary a }
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

instance BlockMarkup a => BlockMarkup (IntroOutroB a) where
  blockMarkup a_ t s (IntroductionB i_) = blockMarkup a_ t s i_
  blockMarkup a_ t s (SummaryB sum_) = blockMarkup a_ t s sum_

data Introduction a = Introduction { _introductionContents :: Vector (Core a) }
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

instance BlockMarkup a => ToMarkup (Introduction a) where
  toMarkup (Introduction i_) = vectorBlockToMarkup "idocIntroduction" id i_

-- FIXME: Needs an icon.
instance BlockMarkup a => BlockMarkup (Introduction a) where
  blockMarkup _ t s i_ = card defaultCardOptions (mTitle "Introduction" t) s "" Nothing (toMarkup i_)

introductionP :: BlockParser a -> IDocParser (Introduction a)
introductionP b_ = Introduction <$> coreBlockP b_

data Summary a = Summary { _summaryContents :: Vector (Core a) }
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

instance BlockMarkup a => ToMarkup (Summary a) where
  toMarkup (Summary s) = vectorBlockToMarkup "idocSummary" id s

-- FIXME: Needs an icon.
instance BlockMarkup a => BlockMarkup (Summary a) where
  blockMarkup _ t s sum_ = card defaultCardOptions (mTitle "Summary" t) s "" Nothing (toMarkup sum_)

summaryP :: BlockParser a -> IDocParser (Summary a)
summaryP b_ = Summary <$> coreBlockP b_

makeLenses ''IntroOutroB

makeLenses ''Introduction
makeLenses ''Summary
