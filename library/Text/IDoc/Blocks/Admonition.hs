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

data AdmonitionB m b = InfoB { _info :: Info m b }
                     | TipB { _tip :: Tip m b }
                     | CautionB { _caution :: Caution m b }
                     | WarningB { _warning :: Warning m b }
                     | SideNoteB { _sidenote :: SideNote m b }
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

instance (MarkupMarkup m, BlockMarkup m (b m)) => BlockMarkup m (AdmonitionB m b) where
  blockMarkup a_ t s (InfoB i_) = blockMarkup a_ t s i_
  blockMarkup a_ t s (TipB tip) = blockMarkup a_ t s tip
  blockMarkup a_ t s (CautionB c) = blockMarkup a_ t s c
  blockMarkup a_ t s (WarningB w) = blockMarkup a_ t s w
  blockMarkup a_ t s (SideNoteB sn) = blockMarkup a_ t s sn

instance (Markupy m, Blocky m (b m)) => Blocky m (AdmonitionB m b) where
  blocky a_ t s (InfoB i_) = blocky a_ t s i_
  blocky a_ t s (TipB tip) = blocky a_ t s tip
  blocky a_ t s (CautionB c) = blocky a_ t s c
  blocky a_ t s (WarningB w) = blocky a_ t s w
  blocky a_ t s (SideNoteB sn) = blocky a_ t s sn

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

data Info m b = Info { _infoContents :: Vector (Core m b) }
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

instance (MarkupMarkup m, BlockMarkup m (b m)) => BlockMarkup m (Info m b) where
  blockMarkup _ title_ sid (Info a_) = card
                                       primaryCardOptions
                                       (mTitle "Info" title_)
                                       sid
                                       infoIcon
                                       Nothing
                                       (decorateAdmonition CInfo $ vectorBlockToMarkup "idocInfo" id a_)

instance (Markupy m, Blocky m (b m)) => Blocky m (Info m b) where
  blocky _ mt msid (Info i_) = infoBlock (mLabel msid title_) (vectorTexy i_)
    where
      title_ = mTitleT mt "Info"

infoP :: IDocParser m b (Info m b)
infoP = Info <$> coreBlockP

data Tip m b = Tip { _tipContents :: Vector (Core m b) }
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

instance (MarkupMarkup m, BlockMarkup m (b m)) => BlockMarkup m (Tip m b) where
  blockMarkup _ title_ sid (Tip t) = card
                                     tipCardOptions
                                     (mTitle "Tip" title_)
                                     sid
                                     tipIcon
                                     Nothing
                                     (decorateAdmonition CPrimary $ vectorBlockToMarkup "idocTip" id t)

instance (Markupy m, Blocky m (b m)) => Blocky m (Tip m b) where
  blocky _ mt msid (Tip t) = tipBlock (mLabel msid title_) (vectorTexy t)
    where
      title_ = mTitleT mt "Tip"

tipP :: IDocParser m b (Tip m b)
tipP = Tip <$> coreBlockP

data Caution m b = Caution { _cautionContents :: Vector (Core m b) }
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

instance (MarkupMarkup m, BlockMarkup m (b m)) => BlockMarkup m (Caution m b) where
  blockMarkup _ title_ sid (Caution c) = card
                                         cautionCardOptions
                                         (mTitle "Caution" title_)
                                         sid
                                         cautionIcon
                                         Nothing
                                         (decorateAdmonition CWarning $ vectorBlockToMarkup "idocCaution" id c)

instance (Markupy m, Blocky m (b m)) => Blocky m (Caution m b) where
  blocky _ mt msid (Caution c) = cautionBlock (mLabel msid title_) (vectorTexy c)
    where
      title_ = mTitleT mt "Caution"

cautionP :: IDocParser m b (Caution m b)
cautionP = Caution <$> coreBlockP

data Warning m b = Warning { _warningContents :: Vector (Core m b) }
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

instance (MarkupMarkup m, BlockMarkup m (b m)) => BlockMarkup m (Warning m b) where
  blockMarkup _ title_ sid (Warning w) = card
                                         warningCardOptions
                                         (mTitle "Info" title_)
                                         sid
                                         warningIcon
                                         Nothing
                                         (decorateAdmonition CDanger $ vectorBlockToMarkup "idocWarning" id w)

instance (Markupy m, Blocky m (b m)) => Blocky m (Warning m b) where
  blocky _ mt msid (Warning w) = warningBlock (mLabel msid title_) (vectorTexy w)
    where 
      title_ = mTitleT mt "Warning"

warningP :: IDocParser m b (Warning m b)
warningP = Warning <$> coreBlockP

data SideNote m b = SideNote { _sideNoteContents :: Vector (Core m b) }
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

instance (MarkupMarkup m, BlockMarkup m (b m)) => BlockMarkup m (SideNote m b) where
  blockMarkup _ title_ sid (SideNote s) = card defaultCardOptions (mTitle "Info" title_) sid (icon "fa-sticky-note-o") Nothing (vectorBlockToMarkup "idocSideNote" id s)

instance (Markupy m, Blocky m (b m)) => Blocky m (SideNote m b) where
  blocky _ mt msid (SideNote s) = sideNoteBlock (mLabel msid title_) (vectorTexy s)
    where
      title_ = mTitleT mt "SideNote"

sideNoteP :: IDocParser m b (SideNote m b)
sideNoteP = SideNote <$> coreBlockP

makeLenses ''AdmonitionB

makeLenses ''Info
makeLenses ''Tip
makeLenses ''Caution
makeLenses ''Warning
makeLenses ''SideNote
