module Text.IDoc.Blocks.Admonition where

import Text.IDoc.Syntax

import Data.Data

import Control.Lens

import ClassyPrelude

data AdmonitionB a = InfoB { _info :: Info a }
                   | TipB { _tip :: Tip a }
                   | CautionB { _caution :: Caution a }
                   | WarningB { _warning :: Warning a }
                   | SideNoteB { _sidenote :: SideNote a }
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

data Info a = Info { _infoContents :: Vector (Core a) }
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

data Tip a = Tip { _tipContents :: Vector (Core a) }
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

data Caution a = Caution { _cautionContents :: Vector (Core a) }
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

data Warning a = Warning { _warningContents :: Vector (Core a) }
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

data SideNote a = SideNote { _sideNoteContents :: Vector (Core a) }
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

makeLenses ''AdmonitionB

makeLenses ''Info
makeLenses ''Tip
makeLenses ''Caution
makeLenses ''Warning
makeLenses ''SideNote
