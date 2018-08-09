module Text.IDoc.Blocks.Code where

import Text.IDoc.Syntax
import Text.IDoc.Parse
import Text.IDoc.Render.Html5.Card
import Text.IDoc.Render.Html5.Icons
import Text.IDoc.Render.Tex

import Text.Blaze.Html5 as B

import Text.LaTeX
import Text.LaTeX.Base.Class

import Data.Data

import Control.Lens

import ClassyPrelude

data Code = Code { _codeContents :: Vector Token }
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

instance ToMarkup Code where
  toMarkup (Code c) = verbatimBlockToMarkup "idocCode" id c

instance MarkupMarkup m => BlockMarkup m Code where
  blockMarkup _ title_ sid c = card defaultCardOptions (mTitle "Code" title_) sid (icon "fa-code") Nothing (B.pre $ toMarkup c)

instance Markupy m => Blocky m Code where
  blocky = codeBlock

codeBlock :: (LaTeXC l, Markupy m) => AttrMap -> Maybe (BlockTitle m) -> Maybe (SetID m) -> Code -> l
codeBlock (AttrMap attrs) _ msid (Code c) = (mLabel msid $
                                             mkCode langName $
                                             raw $ concatMap unToken c) ++
                                            "\n"
  where 
    langName = maybe "" 
                     (\(AttrValue x) -> if x == "idoc" then "" else texy x)
                     (join $ attrs ^.at (AttrName "lang"))

codeP :: IDocParser m b Code
codeP = Code <$> uninterpretedBlockP

makeLenses ''Code
