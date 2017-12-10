{-# LANGUAGE DataKinds #-}

module Text.IDoc.Lang.Ils where

import Control.Lens hiding (Identity)

import Data.Vinyl.CoRec
import Data.Vinyl.Functor

import Text.IDoc.Syntax
import Text.IDoc.Blocks.Admonition
import Text.IDoc.Blocks.BibTex
import Text.IDoc.Blocks.Code
import Text.IDoc.Blocks.Connection
import Text.IDoc.Blocks.Example
import Text.IDoc.Blocks.Exercise
import Text.IDoc.Blocks.FurtherReading
import Text.IDoc.Blocks.IntroOutro
import Text.IDoc.Blocks.Intuition
import Text.IDoc.Blocks.Math
import Text.IDoc.Blocks.Media
import Text.IDoc.Blocks.Prerex
import Text.IDoc.Blocks.Quote
import Text.IDoc.Blocks.Recall

newtype BlockType = BlockType { _unBlockType :: CoRec Identity '[ AdmonitionB BlockType
                                                                , BibTex
                                                                , Code
                                                                , Connection BlockType
                                                                , Example BlockType
                                                                , Exercise BlockType
                                                                , FurtherReading BlockType
                                                                , IntroOutroB BlockType
                                                                , Intuition BlockType
                                                                , DisplayMathB
                                                                , TheoremLikeB BlockType
                                                                , DefinitionLikeB BlockType
                                                                , SimpleMediaB
                                                                , YouTube
                                                                , Prerex
                                                                , Quote
                                                                , Recall BlockType ] }

type IlsDoc = Doc BlockType

makeLenses ''BlockType
