module Text.IDoc.Blocks.IntroOutro where

import Text.IDoc.Syntax

import Data.Data

import Control.Lens

import ClassyPrelude

data IntroOutroB a = IntroductionB { _introduction :: Introduction a }
                   | SummaryB { _summary :: Summary a }
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

data Introduction a = Introduction { _introductionContents :: Vector (Core a) }
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

data Summary a = Summary { _summaryContents :: Vector (Core a) }
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

makeLenses ''IntroOutroB

makeLenses ''Introduction
makeLenses ''Summary
