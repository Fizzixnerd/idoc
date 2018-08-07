module Text.IDoc.Render.Html5.Card where

import Text.IDoc.Render.Html5.Icons

import Text.IDoc.Syntax

import qualified Text.Blaze.Html5 as B
import qualified Text.Blaze.Html5.Attributes as A

import ClassyPrelude as CP


data DefaultCollapseState = Collapsed
                          | Uncollapsed deriving (Eq, Show)

instance B.ToValue DefaultCollapseState where
  toValue Collapsed = ""
  toValue Uncollapsed = "in"

data CardType = CDefault
              | CPrimary
              | CInfo
              | CSuccess
              | CWarning
              | CDanger deriving (Eq, Show)

instance B.ToValue CardType where
  toValue CDefault = "card-default idocCardHeader"
  toValue CPrimary = "card-primary card-inverse idocCardHeaderInverse"
  toValue CInfo    = "card-info card-inverse idocCardHeaderInverse"
  toValue CSuccess = "card-success card-inverse idocCardHeaderInverse"
  toValue CWarning = "card-warning card-inverse idocCardHeaderInverse"
  toValue CDanger  = "card-danger card-inverse idocCardHeaderInverse"

data GridWidth = GridFour
               | GridSix
               | GridEight
               | GridTwelve deriving (Eq, Show)

instance B.ToValue GridWidth where
  toValue GridFour = "col-md-4"
  toValue GridSix = "col-md-6"
  toValue GridEight = "col-md-8"
  toValue GridTwelve = ""

data CardOptions = CardOptions { cardDefaultCollapseState :: DefaultCollapseState
                               , cardType :: CardType
                               , cardGridWidth :: GridWidth
                               } deriving (Eq, Show)

defaultCardOptions :: CardOptions
defaultCardOptions = CardOptions Uncollapsed CDefault GridTwelve

primaryCardOptions :: CardOptions
primaryCardOptions = CardOptions Uncollapsed CPrimary GridTwelve

warningCardOptions :: CardOptions
warningCardOptions = CardOptions Uncollapsed CDanger GridTwelve

cautionCardOptions :: CardOptions
cautionCardOptions = CardOptions Uncollapsed CWarning GridTwelve

infoCardOptions :: CardOptions
infoCardOptions = CardOptions Uncollapsed CInfo GridTwelve

tipCardOptions :: CardOptions
tipCardOptions = infoCardOptions

card :: B.ToValue a =>
        CardOptions
     -> B.Html -- ^ title
     -> Maybe a -- ^ id
     -> Icon -- ^ icon
     -> Maybe B.Html -- ^ footer
     -> B.Html -- ^ body
     -> B.Html
card (CardOptions {..}) title_ id_ icon__ footer_ body_ =
  B.div B.! A.class_ (B.toValue cardGridWidth) $
  B.div B.! A.class_ "card" $
               (B.h5 B.! A.class_ ("card-header " ++ B.toValue cardType) $
                    (mHrefV id_ $
                     B.a B.! A.class_ (if cardType == CDefault then "idocCardHeaderLink" else "idocCardHeaderLinkInverse")
                         B.! B.dataAttribute "toggle" "collapse" $
                     (icon__ ++ " " ++ title_ ++ " " ++ (B.span B.! A.class_ "fa fa-angle-double-down" $ "")))) ++
         (mID' id_ $
          B.div B.! A.class_ "card-collapse show" $
                mfooterify footer_ $ (B.div B.! A.class_ "card-block" $
                                            body_))
  where
    mfooterify Nothing = id
    mfooterify (Just f) = ((flip (++)) (B.div B.! A.class_ "card-footer" $ f))
    mHrefV (Just i') = (B.! A.href ("#" ++ (B.toValue i')))
    mHrefV Nothing  = id
    mID' (Just i') = (B.! A.id (B.toValue i'))
    mID' Nothing = id

mTitle :: B.ToMarkup (BlockTitle m) => B.Html -> Maybe (BlockTitle m) -> B.Html
mTitle defaultTitle blkTitle = maybe defaultTitle B.toMarkup blkTitle
