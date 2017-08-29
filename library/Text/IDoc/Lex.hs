-- | Lex.hs
-- Author: Matt Walker
-- License: https://opensource.org/licenses/BSD-2-Clause
-- Created: Aug 24, 2017
-- Summary: 

{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Text.IDoc.Lex where

import ClassyPrelude as CP

import System.IO

import qualified Data.Map as M
import qualified Data.Set as S
import Data.Maybe

import Data.Text.Encoding as E

import qualified Text.Megaparsec as MP
import Text.Megaparsec.Text

import Text.IDoc.Syntax

reservedPunctuation :: Map Char Token
reservedPunctuation = M.fromList [ ('=', Equals)
                                 , ('<', LAngle)
                                 , ('>', RAngle)
                                 , ('[', LBracket)
                                 , (']', RBracket)
                                 , ('{', LBrace)
                                 , ('}', RBrace)
                                 , (':', Colon)
                                 , ('\n', Newline)
                                 , ('-', Dash)
                                 , ('@', AtSign)
                                 , ('`', BackTick)
                                 , ('*', Asterisk)
                                 , ('_', Underscore)
                                 , ('#', Octothorpe)
                                 , ('~', Tilde)
                                 , ('^', Caret)
                                 , ('"', DoubleQuote)
                                 , ('/', FSlash)
                                 , (',', Comma)
                                 , ('.', Period)
                                 , ('$', DollarSign)
                                 , ('%', PercentSign)
                                 , (';', SemiColon) ]

reservedPunctuationL :: [Char]
reservedPunctuationL = fst <$> (M.toList reservedPunctuation)

reservedPunctuationS :: Set Char
reservedPunctuationS = S.fromList reservedPunctuationL

puncT :: Parser Token
puncT = MP.label "Punctuation" $ do
  (\c -> fromJust $ lookup c reservedPunctuation) <$> (MP.oneOf reservedPunctuationL)

dashT :: Parser Token
dashT = MP.label "Dash" $ do
  const Dash <$> (MP.char '-')

regularTextT :: Parser Token
regularTextT = TextT . fromString <$> (some $ MP.satisfy (\c -> c `notElem` reservedPunctuationS))

token :: Parser Token
token =  MP.try puncT
     <|> regularTextT

tokens :: Parser (Vector Token)
tokens = fromList <$> (many token)

megaMain :: IO ()
megaMain = (withFile "source.idoc" ReadMode 
             (\src -> do
                cnts <- CP.hGetContents src
                case MP.parse (tokens :: Parser (Vector Token)) "source.idoc" (E.decodeUtf8 cnts) of
                  (CP.Right x) -> do
                    --System.IO.hPutStr tex (Data.Text.unpack $ utRender x)
                    CP.print x))
