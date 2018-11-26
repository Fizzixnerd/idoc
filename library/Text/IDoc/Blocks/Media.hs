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

data SimpleMediaB m = ImageB { _image :: Image m }
                    | VideoB { _video :: Video m }
                    | AudioB { _audio :: Audio m }
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

instance MarkupMarkup m => BlockMarkup m (SimpleMediaB m) where
  blockMarkup a_ t s (ImageB i_)  = blockMarkup a_ t s i_
  blockMarkup a_ t s (VideoB v)   = blockMarkup a_ t s v
  blockMarkup a_ t s (AudioB aud) = blockMarkup a_ t s aud

instance Markupy m => Blocky m (SimpleMediaB m) where
  blocky a_ t s (ImageB i_)  = blocky a_ t s i_
  blocky a_ t s (VideoB v)   = blocky a_ t s v
  blocky a_ t s (AudioB aud) = blocky a_ t s aud

instance CheckLinks m b m => CheckLinks m b (SimpleMediaB m) where
  checkLinks constraints container (ImageB i_) = checkLinks constraints container i_
  checkLinks constraints container (VideoB v) = checkLinks constraints container v
  checkLinks constraints container (AudioB aud) = checkLinks constraints container aud

data Image m = Image { _imageLink :: Link m
                     , _imageCaption :: Maybe (Vector (SimpleCore m)) }
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

instance MarkupMarkup m => BlockMarkup m (Image m) where
  blockMarkup _ t s (Image il cap) = card
                                     defaultCardOptions
                                     (mTitle "Image" t)
                                     s
                                     (icon "fa-image")
                                     (vectorBlockToMarkup "idocImageCaption" id <$> cap)
                                     (img ! class_ "idocImage img-responsive"
                                          ! src (toValue il))

instance CheckLinks m b m => CheckLinks m b (Image m) where
  checkLinks constraints container (Image _ cap) =
    maybe mempty (concatMap (checkLinks constraints container)) cap

data Video m = Video { _videoLink :: Link m
                     , _videoCaption :: Maybe (Vector (SimpleCore m)) }
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

-- FIXME: Find an icon for this.
instance MarkupMarkup m => BlockMarkup m (Video m) where
  blockMarkup _ t s (Video vl cap) = card
                                     defaultCardOptions
                                     (mTitle "Video" t)
                                     s
                                     ""
                                     (vectorBlockToMarkup "idocVideoCaption" id <$> cap)
                                     (video ! class_ "idocVideo"
                                            ! controls "true"
                                            ! src (toValue vl) $
                                            "")

instance CheckLinks m b m => CheckLinks m b (Video m) where
  checkLinks constraints container (Video _ cap) =
    maybe mempty (concatMap (checkLinks constraints container)) cap

data YouTube m = YouTube { _youTubeLink :: Link m
                         , _youTubeCaption :: Maybe (Vector (SimpleCore m)) }
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

-- FIXME: The link location needs to be fixed.
instance MarkupMarkup m => BlockMarkup m (YouTube m) where
  blockMarkup _ t s (YouTube yl cap) = card
                                       defaultCardOptions
                                       (mTitle "YouTube" t)
                                       s
                                       (icon "fa-youtube")
                                       (vectorBlockToMarkup "idocYouTubeCaption" id <$> cap)
                                       (div ! class_ "embed-responsive embed-responsive-16by9" $
                                            iframe ! class_ "idocYouTubeEmbed embed-responsive-item"
                                                   ! allowFullscreen "true"
                                                   ! src (toValue ("https://www.youtube.com/embed/" :: Text) ++ (yl^.linkLocation.idBase.to (intersperse (IDBase "/")).to (concatMap _unIDBase).to toValue)) $
                                                   "")
    where allowFullscreen = customAttribute "allowfullscreen"

instance CheckLinks m b m => CheckLinks m b (YouTube m) where
  checkLinks constraints container (YouTube _ cap) =
    maybe mempty (concatMap (checkLinks constraints container)) cap

data Audio m = Audio
  { _audioLink :: Link m
  , _audioCaption :: Maybe (Vector (SimpleCore m))
  } deriving (Eq, Ord, Show, Data, Typeable, Generic)

instance MarkupMarkup m => BlockMarkup m (Audio m) where
  blockMarkup _ t s (Audio al cap) = card
                                     defaultCardOptions
                                     (mTitle "Audio" t)
                                     s
                                     (icon "fa-headphones")
                                     (vectorBlockToMarkup "idocAudioCaption" id <$> cap)
                                     (div ! class_ "idocAudioContainer d-flex justify-content-center" $
                                         (audio ! class_ "idocAudio"
                                                ! controls "true"
                                                ! src (toValue al) $
                                                ""))

instance CheckLinks m b m => CheckLinks m b (Audio m) where
  checkLinks constraints container (Audio _ cap) =
    maybe mempty (concatMap (checkLinks constraints container)) cap


imageP :: IDocParser m b (Image m)
imageP = do
  (l, sc) <- linkBlockWithOptionalP
  return $ Image l sc

videoP :: IDocParser m b (Video m)
videoP = do
  (l, sc) <- linkBlockWithOptionalP
  return $ Video l sc

youTubeP :: IDocParser m b (YouTube m)
youTubeP = do
  (l, sc) <- linkBlockWithOptionalP
  return $ YouTube l sc

audioP :: IDocParser m b (Audio m)
audioP = do
  (l, sc) <- linkBlockWithOptionalP
  return $ Audio l sc

instance Markupy m => Blocky m (YouTube m) where
  blocky _ _ msid (YouTube lnk mcaption) = mLabel msid $
                                        texy lnk ++
                                        maybe "" vectorTexy mcaption

instance Markupy m => Blocky m (Image m) where
  blocky _ _ msid (Image lnk mcaption) = mLabel msid $
                                      texy lnk ++
                                      maybe "" vectorTexy mcaption

instance Markupy m => Blocky m (Video m) where
  blocky _ _ msid (Video lnk mcaption) = mLabel msid $
                                      texy lnk ++
                                      maybe "" vectorTexy mcaption

instance Markupy m => Blocky m (Audio m) where
  blocky _ _ msid (Audio lnk mCaption) = mLabel msid $
                                         texy lnk ++
                                         maybe "" vectorTexy mCaption

makeLenses ''SimpleMediaB

makeLenses ''Image
makeLenses ''Video
makeLenses ''YouTube
makeLenses ''Audio
