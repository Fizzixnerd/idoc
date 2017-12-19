module Text.IDoc.Blocks.Prerex where

import Text.IDoc.Syntax
import Text.IDoc.Parse
import Text.IDoc.Render.Html5.Card
import Text.IDoc.Render.Html5.Icons
import Text.IDoc.Render.Tex

import Text.Blaze.Html5 as B
import Text.Blaze.Html5.Attributes as A

import Text.LaTeX
import Text.LaTeX.Packages.Hyperref as H

import Data.Data

import Control.Lens

import ClassyPrelude hiding (div)

data Prerex = Prerex { _prerexContents :: Vector PrerexItem }
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

prerexP :: IDocParser Prerex
prerexP = Prerex <$> do
  blockStarterP
  someTill blockEnderP $ do
    x <- prerexItemP
    newlineP
    return x

data PrerexItem = PrerexItem { _prerexItemPath :: ID
                             , _prerexItemDescription :: Vector SimpleCore
                             }
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

prerexItemP :: IDocParser PrerexItem
prerexItemP = do
  path <- idP
  desc <- markupContentsP
  return $ PrerexItem { _prerexItemPath = path
                      , _prerexItemDescription = desc
                      }

makeLenses ''Prerex
makeLenses ''PrerexItem

instance ToMarkup Prerex where
  toMarkup p_ = div ! class_ "idocPrerex" $
                    concatMap toMarkup (p_^.prerexContents)

-- FIXME: Find an icon for this.
instance BlockMarkup Prerex where
  blockMarkup _ t s p_ = card 
                         primaryCardOptions
                         (mTitle "Prerex" t)
                         s
                         ""
                         Nothing
                         (toMarkup p_)

instance Blocky Prerex where
  block _ mt msid (Prerex ps) = subsubsection (mLabel msid title_) ++ vectorTexy ps
    where
      title_ = mTitleT mt "Prerex"

instance ToMarkup PrerexItem where
  toMarkup p_ = card (defaultCardOptions { cardType = CInfo
                                         , cardDefaultCollapseState = Collapsed
                                         })
                     (toMarkup $ p_^.prerexItemPath)
                     (Just $ p_^.prerexItemPath)
                     prerexItemIcon
                     (Just $ a ! class_ "idocPrerexItemLink"
                               ! A.href (p_^.prerexItemPath.to toValue) $
                               "Go to " ++ (p_^.prerexItemPath.to toMarkup))
                     (concatMap toMarkup $ p_^.prerexItemDescription)

instance Texy PrerexItem where
  texy p_ = (H.href [] "" $ texy $ fromBack $ p_^.prerexItemPath) ++
            ": " ++
            (concatMap texy $ p_^.prerexItemDescription) ++
            newline
    where fromBack id_ = "http://www.independentlearning.science/tiki/" ++ 
                         (concatMap unIDBase $ intersperse (IDBase "/") (id_^.idBase))

