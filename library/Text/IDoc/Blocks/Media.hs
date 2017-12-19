module Text.IDoc.Blocks.Media where

import Text.IDoc.Syntax
import Text.IDoc.Parse
import Text.IDoc.Render.Html5.Card
import Text.IDoc.Render.Html5.Icons
import Text.IDoc.Render.Tex

import Text.Blaze.Html5
import Text.Blaze.Html5.Attributes hiding (id, icon)

import Text.LaTeX

import Control.Lens

import Data.Data

import ClassyPrelude hiding (div)

data SimpleMediaB = ImageB { _image :: Image }
                  | VideoB { _video :: Video }
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

instance BlockMarkup SimpleMediaB where
  blockMarkup a_ t s (ImageB i_) = blockMarkup a_ t s i_
  blockMarkup a_ t s (VideoB v)  = blockMarkup a_ t s v

instance Blocky SimpleMediaB where
  block a_ t s (ImageB i_) = block a_ t s i_
  block a_ t s (VideoB v)  = block a_ t s v

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

instance Blocky YouTube where
  block _ _ msid (YouTube lnk mcaption) = mLabel msid $ 
                                        texy lnk ++
                                        maybe "" vectorTexy mcaption

instance Blocky Image where
  block _ _ msid (Image lnk mcaption) = mLabel msid $
                                      texy lnk ++
                                      maybe "" vectorTexy mcaption

instance Blocky Video where
  block _ _ msid (Video lnk mcaption) = mLabel msid $ 
                                      texy lnk ++
                                      maybe "" vectorTexy mcaption

makeLenses ''SimpleMediaB

makeLenses ''Image
makeLenses ''Video
makeLenses ''YouTube

