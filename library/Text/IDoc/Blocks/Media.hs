module Text.IDoc.Blocks.Media where

import Text.IDoc.Syntax
import Text.IDoc.Parse
import Text.IDoc.Render.Html5.Card
import Text.IDoc.Render.Html5.Icons

import Text.Blaze.Html5
import Text.Blaze.Html5.Attributes hiding (id, icon)

import Control.Lens

import Data.Data

import ClassyPrelude hiding (div)

data SimpleMediaB = ImageB { _image :: Image }
                  | VideoB { _video :: Video }
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

instance BlockMarkup SimpleMediaB where
  blockMarkup a_ t s (ImageB i_) = blockMarkup a_ t s i_
  blockMarkup a_ t s (VideoB v)  = blockMarkup a_ t s v

data Image = Image { _imageLink :: Link
                   , _imageCaption :: Maybe (Vector SimpleCore) }
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

instance BlockMarkup Image where
  blockMarkup _ t s (Image il cap) = card 
                                     defaultCardOptions
                                     (mTitle "Image" t)
                                     s
                                     (icon "fa-image")
                                     (vectorBlockToMarkup "idocImageCaption" id <$> cap)
                                     (img ! class_ "idocImage img-responsive"
                                          ! src (toValue $ il^.linkLocation))

data Video = Video { _videoLink :: Link
                   , _videoCaption :: Maybe (Vector SimpleCore) }
  deriving (Eq, Ord, Show, Data, Typeable, Generic)


-- FIXME: Find an icon for this.
instance BlockMarkup Video where
  blockMarkup _ t s (Video vl cap) = card
                                     defaultCardOptions
                                     (mTitle "Video" t)
                                     s
                                     ""
                                     (vectorBlockToMarkup "idocVideoCaption" id <$> cap)
                                     (video ! class_ "idocVideo"
                                            ! controls "true"
                                            ! src (toValue $ vl^.linkLocation) $
                                            "")

data YouTube = YouTube { _youTubeLink :: Link
                       , _youTubeCaption :: Maybe (Vector SimpleCore) }
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

-- FIXME: The link location needs to be fixed.
instance BlockMarkup YouTube where
  blockMarkup _ t s (YouTube yl cap) = card
                                       defaultCardOptions
                                       (mTitle "YouTube" t)
                                       s
                                       (icon "fa-youtube")
                                       (vectorBlockToMarkup "idocYouTubeCaption" id <$> cap)
                                       (div ! class_ "embed-responsive embed-responsive-16by9" $
                                            iframe ! class_ "idocYouTubeEmbed embed-responsive-item"
                                                   ! allowFullscreen "true"
                                                   ! src (toValue $ yl^.linkLocation) $
                                                   "")
    where allowFullscreen = customAttribute "allowfullscreen"

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

