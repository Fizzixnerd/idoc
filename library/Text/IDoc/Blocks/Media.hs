module Text.IDoc.Blocks.Media where

import Text.IDoc.Syntax
import Text.IDoc.Parse

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

imageP :: IDocParser Image
imageP = do
  (l, sc) <- linkBlockWithOptionalP
  return $ Image l sc

videoP :: IDocParser Video
videoP = do
  (l, sc) <- linkBlockWithOptionalP
  return $ Video l sc

youTubeP :: IDocParser YouTube
youTubeP = do
  (l, sc) <- linkBlockWithOptionalP
  return $ YouTube l sc

makeLenses ''SimpleMediaB

makeLenses ''Image
makeLenses ''Video
makeLenses ''YouTube

