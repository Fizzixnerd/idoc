module Text.IDoc.Blocks.Media where

import Text.IDoc.Syntax

import Control.Lens

import Data.Data

import ClassyPrelude

data SimpleMediaB = ImageB { _image :: Image }
                  | VideoB { _video :: Video }
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

data Image = Image { _imageLink :: Link
                   , _imageCaption :: Maybe (Vector SimpleCore) }
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

data Video = Video { _videoLink :: Link
                   , _videoCaption :: Maybe (Vector SimpleCore) }
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

data YouTube = YouTube { _youTubeLink :: Link
                       , _youTubeCaption :: Maybe (Vector SimpleCore) }
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

makeLenses ''SimpleMediaB

makeLenses ''Image
makeLenses ''Video
makeLenses ''YouTube

