module Text.IDoc.Blocks.Admonition where

import Text.IDoc.Syntax
import Text.IDoc.Parse
import Text.IDoc.Render.Html5.Card
import Text.IDoc.Render.Html5.Icons
import Text.IDoc.Render.Tex

import Text.Blaze.Html5 as B
import Text.Blaze.Html5.Attributes as A hiding (id, icon)

import Data.Data

import Control.Lens

import ClassyPrelude hiding (span)

data AdmonitionB a = InfoB { _info :: Info a }
                   | TipB { _tip :: Tip a }
                   | CautionB { _caution :: Caution a }
                   | WarningB { _warning :: Warning a }
                   | SideNoteB { _sidenote :: SideNote a }
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

instance BlockMarkup a => BlockMarkup (AdmonitionB a) where
  blockMarkup a_ t s (InfoB i_) = blockMarkup a_ t s i_
  blockMarkup a_ t s (TipB tip) = blockMarkup a_ t s tip
  blockMarkup a_ t s (CautionB c) = blockMarkup a_ t s c
  blockMarkup a_ t s (WarningB w) = blockMarkup a_ t s w
  blockMarkup a_ t s (SideNoteB sn) = blockMarkup a_ t s sn

instance Blocky a => Blocky (AdmonitionB a) where
  block a_ t s (InfoB i_) = block a_ t s i_
  block a_ t s (TipB tip) = block a_ t s tip
  block a_ t s (CautionB c) = block a_ t s c
  block a_ t s (WarningB w) = block a_ t s w
  block a_ t s (SideNoteB sn) = block a_ t s sn

decorateAdmonition :: CardType -> Html -> Html
decorateAdmonition pt cnt = (B.span ! class_ 
                             ("fa " ++
                              faIcon ++
                              " fa-4x fa-pull-left") $ "") ++ cnt
  where
    faIcon = case pt of
      CInfo -> "fa-info-circle"
      CDanger -> "fa-exclamation-circle"
      CWarning -> "fa-exclamation-triangle"
      CPrimary -> "fa-lightbulb-o"
      _ -> error "I can't decorate like that!"

data Info a = Info { _infoContents :: Vector (Core a) }
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

instance BlockMarkup a => ToMarkup (Info a) where
  toMarkup (Info a_) = decorateAdmonition CInfo $
                       vectorBlockToMarkup "idocInfo" id a_

instance BlockMarkup a => BlockMarkup (Info a) where
  blockMarkup _ title_ sid i_ = card primaryCardOptions (mTitle "Info" title_) sid infoIcon Nothing (toMarkup i_)

instance Blocky a => Blocky (Info a) where
  block _ mt msid (Info i_) = infoBlock (mLabel msid title_) (vectorTexy i_)
    where
      title_ = mTitleT mt "Info"

infoP :: BlockParser a -> IDocParser (Info a)
infoP b_ = Info <$> coreBlockP b_

data Tip a = Tip { _tipContents :: Vector (Core a) }
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

instance BlockMarkup a => ToMarkup (Tip a) where
  toMarkup (Tip a_) = decorateAdmonition CPrimary $ 
                      vectorBlockToMarkup "idocTip" id a_

instance BlockMarkup a => BlockMarkup (Tip a) where
  blockMarkup _ title_ sid t = card tipCardOptions (mTitle "Tip" title_) sid tipIcon Nothing (toMarkup t)

instance Blocky a => Blocky (Tip a) where
  block _ mt msid (Tip t) = tipBlock (mLabel msid title_) (vectorTexy t)
    where
      title_ = mTitleT mt "Tip"

tipP :: BlockParser a -> IDocParser (Tip a)
tipP b_ = Tip <$> coreBlockP b_

data Caution a = Caution { _cautionContents :: Vector (Core a) }
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

instance BlockMarkup a => ToMarkup (Caution a) where
  toMarkup (Caution a_) = decorateAdmonition CWarning $ 
                          vectorBlockToMarkup "idocCaution" id a_

instance BlockMarkup a => BlockMarkup (Caution a) where
  blockMarkup _ title_ sid c = card cautionCardOptions (mTitle "Caution" title_) sid cautionIcon Nothing (toMarkup c)

instance Blocky a => Blocky (Caution a) where
  block _ mt msid (Caution c) = cautionBlock (mLabel msid title_) (vectorTexy c)
    where
      title_ = mTitleT mt "Caution"

cautionP :: BlockParser a -> IDocParser (Caution a)
cautionP b_ = Caution <$> coreBlockP b_

data Warning a = Warning { _warningContents :: Vector (Core a) }
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

instance BlockMarkup a => ToMarkup (Warning a) where
  toMarkup (Warning a_) = decorateAdmonition CDanger $ 
                          vectorBlockToMarkup "idocWarning" id a_

instance BlockMarkup a => BlockMarkup (Warning a) where
  blockMarkup _ title_ sid w = card warningCardOptions (mTitle "Info" title_) sid warningIcon Nothing (toMarkup w)

instance Blocky a => Blocky (Warning a) where
  block _ mt msid (Warning w) = warningBlock (mLabel msid title_) (vectorTexy w)
    where 
      title_ = mTitleT mt "Warning"

warningP :: BlockParser a -> IDocParser (Warning a)
warningP b_ = Warning <$> coreBlockP b_

data SideNote a = SideNote { _sideNoteContents :: Vector (Core a) }
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

instance BlockMarkup a => ToMarkup (SideNote a) where
  toMarkup (SideNote s) = vectorBlockToMarkup "idocSideNote" id s

instance BlockMarkup a => BlockMarkup (SideNote a) where
  blockMarkup _ title_ sid sn = card defaultCardOptions (mTitle "Info" title_) sid (icon "fa-sticky-note-o") Nothing (toMarkup sn)

instance Blocky a => Blocky (SideNote a) where
  block _ mt msid (SideNote s) = sideNoteBlock (mLabel msid title_) (vectorTexy s)
    where
      title_ = mTitleT mt "SideNote"

sideNoteP :: BlockParser a -> IDocParser (SideNote a)
sideNoteP b_ = SideNote <$> coreBlockP b_

makeLenses ''AdmonitionB

makeLenses ''Info
makeLenses ''Tip
makeLenses ''Caution
makeLenses ''Warning
makeLenses ''SideNote
