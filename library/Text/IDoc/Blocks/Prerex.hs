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

data Prerex m = Prerex { _prerexContents :: Vector (PrerexItem m) }
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

prerexP :: MarkupParser m -> IDocParser (Prerex m)
prerexP m = Prerex <$> do
  blockStarterP
  someTill blockEnderP $ do
    x <- prerexItemP m
    newlineP
    return x

data PrerexItem m = PrerexItem { _prerexItemPath :: ID
                               , _prerexItemDescription :: Vector (SimpleCore m)
                               }
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

prerexItemP :: MarkupParser m -> IDocParser (PrerexItem m)
prerexItemP m = do
  path <- idP
  desc <- markupContentsP m
  return $ PrerexItem { _prerexItemPath = path
                      , _prerexItemDescription = desc
                      }

makeLenses ''Prerex
makeLenses ''PrerexItem

-- FIXME: Find an icon for this.
instance MarkupMarkup m => BlockMarkup m (Prerex m) where
  blockMarkup _ t s p_ = card 
                         primaryCardOptions
                         (mTitle "Prerex" t)
                         s
                         ""
                         Nothing
                         (div ! class_ "idocPrerex" $
                          concatMap toMarkup (p_^.prerexContents))

instance Markupy m => Blocky m (Prerex m) where
  blocky _ mt msid (Prerex ps) = subsubsection (mLabel msid title_) ++ vectorTexy ps
    where
      title_ = mTitleT mt "Prerex"

instance MarkupMarkup m => ToMarkup (PrerexItem m) where
  toMarkup p_ = card (defaultCardOptions { cardType = CInfo
                                         , cardDefaultCollapseState = Collapsed
                                         })
                     itemPath
                     (Just $ p_^.prerexItemPath)
                     prerexItemIcon
                     (Just $ a ! class_ "idocPrerexItemLink"
                               ! A.href (p_^.prerexItemPath.to toValue) $
                               "Go to " ++ itemPath)
                     (concatMap toMarkup $ p_^.prerexItemDescription)
    where
      itemPath = toMarkup $ "https://www.independentlearning.science/tiki/" ++
                 (concatMap unIDBase $ intersperse (IDBase "/") (p_^.prerexItemPath.idBase))


instance Markupy m => Texy (PrerexItem m) where
  texy p_ = (H.href [] "" $ texy $ fromBack $ p_^.prerexItemPath) ++
            ": " ++
            (concatMap texy $ p_^.prerexItemDescription) ++
            newline
    where fromBack id_ = "http://www.independentlearning.science/tiki/" ++ 
                         (concatMap unIDBase $ intersperse (IDBase "/") (id_^.idBase))

