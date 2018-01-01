module Text.IDoc.Blocks.IntroOutro where

import Text.IDoc.Syntax
import Text.IDoc.Parse
import Text.IDoc.Render.Html5.Card
import Text.IDoc.Render.Tex

import Text.LaTeX

import Data.Data

import Control.Lens

import ClassyPrelude

data IntroOutroB m b = IntroductionB { _introduction :: Introduction m b }
                     | SummaryB { _summary :: Summary m b }
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

instance (MarkupMarkup m, BlockMarkup m (b m)) => BlockMarkup m (IntroOutroB m b) where
  blockMarkup a_ t s (IntroductionB i_) = blockMarkup a_ t s i_
  blockMarkup a_ t s (SummaryB sum_) = blockMarkup a_ t s sum_

data Introduction m b = Introduction { _introductionContents :: Vector (Core m b) }
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

-- FIXME: Needs an icon.
instance (MarkupMarkup m, BlockMarkup m (b m)) => BlockMarkup m (Introduction m b) where
  blockMarkup _ t s (Introduction i_) = card
                                        defaultCardOptions
                                        (mTitle "Introduction" t)
                                        s
                                        ""
                                        Nothing
                                        (vectorBlockToMarkup "idocIntroduction" id i_)

instance (Markupy m, Blocky m (b m)) => Blocky m (Introduction m b) where
  blocky _ mt msid (Introduction i_) = subsection (mLabel msid title_) ++ vectorTexy i_
    where
      title_ = mTitleT mt "Introduction"

introductionP :: IDocParser m b (Introduction m b)
introductionP = Introduction <$> coreBlockP

data Summary m b = Summary { _summaryContents :: Vector (Core m b) }
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

-- FIXME: Needs an icon.
instance (MarkupMarkup m, BlockMarkup m (b m)) => BlockMarkup m (Summary m b) where
  blockMarkup _ t s (Summary sum_) = card
                                     defaultCardOptions
                                     (mTitle "Summary" t)
                                     s
                                     ""
                                     Nothing
                                     (vectorBlockToMarkup "idocSummary" id sum_)

instance (Markupy m, Blocky m (b m)) => Blocky m (Summary m b) where
  blocky _ mt msid (Summary s) = (subsection $ mLabel msid title_) ++
                                vectorTexy s
    where
      title_ = mTitleT mt "Summary"

summaryP :: IDocParser m b (Summary m b)
summaryP = Summary <$> coreBlockP

makeLenses ''IntroOutroB

makeLenses ''Introduction
makeLenses ''Summary
