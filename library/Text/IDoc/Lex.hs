-- | Lex.hs
-- Author: Matt Walker
-- License: https://opensource.org/licenses/BSD-2-Clause
-- Created: Aug 24, 2017
-- Summary:

module Text.IDoc.Lex where

import ClassyPrelude as CP

import qualified Data.Map as M
import qualified Data.Set as S
import Data.Maybe

import Data.Void

import qualified Text.Megaparsec as MP
import Text.Megaparsec
import Text.Megaparsec.Char

import Text.IDoc.Syntax as S

reservedPunctuation :: Map Char S.Token
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
reservedPunctuationL = fst <$> M.toList reservedPunctuation

reservedPunctuationS :: Set Char
reservedPunctuationS = S.fromList reservedPunctuationL

mkDTokenP :: Parsec (ErrorFancy Void) Text S.Token -> Parsec (ErrorFancy Void) Text DToken
mkDTokenP p = do
  MP.SourcePos _ r1 c1 <- MP.getPosition
  x <- p
  MP.SourcePos _ r2 c2 <- MP.getPosition
  let di = DebugInfo (MP.unPos r1, MP.unPos c1) (MP.unPos r2, MP.unPos c2)
  return $ DebugToken di x

puncT :: Parsec (ErrorFancy Void) Text S.Token
puncT = MP.label "Punctuation" $
        (\c -> fromJust $ lookup c reservedPunctuation) <$>
        oneOf reservedPunctuationL

dPuncT :: Parsec (ErrorFancy Void) Text DToken
dPuncT = MP.label "Punctuation" $ mkDTokenP puncT

dashT :: Parsec (ErrorFancy Void) Text S.Token
dashT = MP.label "Dash" $
  const Dash <$> char '-'

regularTextT :: Parsec (ErrorFancy Void) Text S.Token
regularTextT = TextT . fromString <$> (CP.some $ satisfy (`notElem` reservedPunctuationS))

dRegularTextT :: Parsec (ErrorFancy Void) Text DToken
dRegularTextT = mkDTokenP regularTextT

token :: Parsec (ErrorFancy Void) Text S.Token
token =  MP.try puncT
     <|>        regularTextT

dToken :: Parsec (ErrorFancy Void) Text DToken
dToken =  MP.try dPuncT
      <|>        dRegularTextT

tokens :: Parsec (ErrorFancy Void) Text (Vector S.Token)
tokens = fromList <$> (CP.many Text.IDoc.Lex.token)

dTokens :: Parsec (ErrorFancy Void) Text IDocTokenStream
dTokens = IDocTokenStream <$> fromList <$> (CP.many dToken)
