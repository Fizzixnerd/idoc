module Text.IDoc.Blocks.Code where

import Text.IDoc.Syntax
import Text.IDoc.Parse
import Text.IDoc.Render.Html5.Card
import Text.IDoc.Render.Html5.Icons
import Text.IDoc.Render.Tex

import Text.Blaze.Html5

import Text.LaTeX
import Text.LaTeX.Base.Class

import Data.Data

import Control.Lens

import ClassyPrelude

data Code = Code { _codeContents :: Vector Token }
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

instance ToMarkup Code where
  toMarkup (Code c) = verbatimBlockToMarkup "idocCode" id c

instance BlockMarkup Code where
  blockMarkup _ title_ sid c = card defaultCardOptions (mTitle "Code" title_) sid (icon "fa-code") Nothing (toMarkup c)

instance Blocky Code where
  block = codeBlock

codeBlock :: LaTeXC l => AttrMap -> Maybe BlockTitle -> Maybe SetID -> Code -> l
codeBlock (AttrMap attrs) _ msid (Code c) = (mLabel msid $
                                             mkCode langName $
                                             raw $ concatMap unToken c) ++
                                            "\n"
  where 
    langName = maybe "" 
                     (\(AttrValue x) -> if x == "idoc" then "" else texy x)
                     (join $ attrs ^.at (AttrName "lang"))

codeP :: IDocParser Code
codeP = Code <$> uninterpretedBlockP

makeLenses ''Code
