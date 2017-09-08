{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | Lex.hs
-- Author: Matt Walker
-- License: https://opensource.org/licenses/BSD-2-Clause
-- Created: Aug 24, 2017
-- Summary: 

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
                                 , (';', SemiColon)
                                 , ('\\', BSlash)
                                 , ('+', Plus) ]

reservedPunctuationL :: [Char]
reservedPunctuationL = fst <$> (M.toList reservedPunctuation)

reservedPunctuationS :: Set Char
reservedPunctuationS = S.fromList reservedPunctuationL

mkDTokenP :: Parser Token -> Parser DToken
mkDTokenP p = do
  MP.SourcePos _ r1 c1 <- MP.getPosition
  x <- p
  MP.SourcePos _ r2 c2 <- MP.getPosition
  let di = DebugInfo (MP.unPos r1, MP.unPos c1) (MP.unPos r2, MP.unPos c2)
  return $ DebugToken di x

puncT :: Parser Token
puncT = MP.label "Punctuation" $ do
  (\c -> fromJust $ lookup c reservedPunctuation) <$> (MP.oneOf reservedPunctuationL)

dPuncT :: Parser DToken
dPuncT = MP.label "Punctuation" $ mkDTokenP puncT

dashT :: Parser Token
dashT = MP.label "Dash" $ do
  const Dash <$> (MP.char '-')

regularTextT :: Parser Token
regularTextT = TextT . fromString <$> (some $ MP.satisfy (\c -> c `notElem` reservedPunctuationS))

dRegularTextT :: Parser DToken
dRegularTextT = mkDTokenP regularTextT

token :: Parser Token
token =  MP.try puncT
     <|>        regularTextT

dToken :: Parser DToken
dToken =  MP.try dPuncT
      <|>        dRegularTextT

tokens :: Parser (Vector Token)
tokens = fromList <$> (many token)

dTokens :: Parser IDocTokenStream
dTokens = IDocTokenStream <$> fromList <$> (many dToken)

megaMain :: IO ()
megaMain = withFile "source.idoc" ReadMode 
             (\src -> do
                cnts <- CP.hGetContents src
                case MP.parse (tokens :: Parser (Vector Token)) "source.idoc" (E.decodeUtf8 cnts) of
                  (CP.Right x) -> CP.print x)
