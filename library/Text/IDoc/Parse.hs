-- | Parse2.hs
-- Author: Matt Walker
-- License: https://opensource.org/licenses/BSD-2-Clause
-- Created: Aug 25, 2017
-- Summary: 
module Text.IDoc.Parse where

import ClassyPrelude as CP

import Data.Char
import qualified Data.Map as M
import qualified Data.Vector as V
import Data.Void

import qualified Text.Megaparsec as MP

import qualified Text.IDoc.Syntax as S
import qualified Text.IDoc.Lex as L

import Control.Lens

type IDocParseError = MP.ParseError S.DToken Void

data IDocReadState m b = IDocReadState { _rel    :: Text
                                       , _imgRel :: Text
                                       , _audioRel :: Text
                                       , _mp     :: MarkupParser m b
                                       , _bp     :: BlockParser m b
                                       }

newtype IDocParser m b a = IDocParser { unIDocParser :: MP.ParsecT Void S.IDocTokenStream (ReaderT (IDocReadState m b) Identity)  a }
  deriving (Functor, Applicative, Monad, MonadReader (IDocReadState m b), MonadPlus,
            MP.MonadParsec Void S.IDocTokenStream, Alternative)

type BlockTypeName = Text
type InnerBlockParser m b = MarkupParser m b -> BlockTypeName -> IDocParser m b (b m)
type BlockParser m b = BlockTypeName -> IDocParser m b (b m)

type MarkupTypeName = Text
type MarkupParser m b = MarkupTypeName -> IDocParser m b m

makeLenses ''IDocReadState

manyV :: IDocParser m b a -> IDocParser m b (Vector a)
manyV x = fromList <$> many x

someV :: IDocParser m b a -> IDocParser m b (Vector a)
someV x = fromList <$> some x

sepEndByV :: IDocParser m b a -> IDocParser m b sep -> IDocParser m b (Vector a)
sepEndByV x sep = fromList <$> MP.sepEndBy x sep

sepEndBy1V :: IDocParser m b a -> IDocParser m b sep -> IDocParser m b (Vector a)
sepEndBy1V x sep = fromList <$> MP.sepEndBy1 x sep

sepByV :: IDocParser m b a -> IDocParser m b sep -> IDocParser m b (Vector a)
sepByV x sep = fromList <$> MP.sepBy x sep

sepBy1V :: IDocParser m b a -> IDocParser m b sep -> IDocParser m b (Vector a)
sepBy1V x sep = fromList <$> MP.sepBy1 x sep

manyTween :: IDocParser m b l -> IDocParser m b r -> IDocParser m b a -> IDocParser m b (Vector a)
manyTween l r x = do
  void l
  xs <- manyV $ do
    MP.notFollowedBy r
    x
  void r
  return xs

someTween :: IDocParser m b l -> IDocParser m b r -> IDocParser m b a -> IDocParser m b (Vector a)
someTween l r x = do
  void l
  xs <- someV $ do
    MP.notFollowedBy r
    x
  void r
  return xs

someTill :: IDocParser m b ender -> IDocParser m b a -> IDocParser m b (Vector a)
someTill e x = do
  xs <- someV $ do
    MP.notFollowedBy e
    x
  void e
  return xs

someTill' :: IDocParser m b ender -> IDocParser m b a -> IDocParser m b (Vector a, ender)
someTill' e x = do
  xs <- someV $ do
    MP.notFollowedBy e
    x
  e_ <- e
  return (xs, e_)

manyTill :: IDocParser m b ender -> IDocParser m b a -> IDocParser m b (Vector a)
manyTill e x = do
  xs <- manyV $ do
    MP.notFollowedBy e
    x
  void e
  return xs

satisfy :: (MP.MonadParsec e s m, MP.Token s ~ S.DToken) => (S.Token -> Bool) -> m S.Token
satisfy f = MP.token test mempty
  where
    test S.DebugToken { S._dtToken = x } = if f x
                                           then Just x
                                           else Nothing

tokenP :: S.Token -> IDocParser m b S.Token
tokenP t = Text.IDoc.Parse.satisfy (== t) MP.<?> (show t)

anyTokenP :: IDocParser m b S.Token
anyTokenP = Text.IDoc.Parse.satisfy (const True) MP.<?> "Any Token"

bracedTokensP :: IDocParser m b (Vector S.Token)
bracedTokensP = do
  t <- textP
  if not $ all isSpace t
    then fail "Found non-spaces where there shouldn't be."
    else do
    void $ tokenP S.LBrace
    someV $ do
      MP.notFollowedBy (tokenP S.RBrace)
      anyTokenP

quotedTokensP :: IDocParser m b (Vector S.Token)
quotedTokensP = do
  t <- textP
  if not $ all isSpace t
    then fail "Found non-spaces where there shouldn't be."
    else do
    void $ tokenP S.DoubleQuote
    someV $ do
      MP.notFollowedBy (tokenP S.DoubleQuote)
      anyTokenP

newlineP :: IDocParser m b ()
newlineP = void $ tokenP S.Newline

dashP :: IDocParser m b ()
dashP = void $ tokenP S.Dash

plusP :: IDocParser m b ()
plusP = void $ tokenP S.Plus

fSlashP :: IDocParser m b ()
fSlashP = void $ tokenP S.FSlash

octothorpeP :: IDocParser m b ()
octothorpeP = void $ tokenP S.Octothorpe

asteriskP :: IDocParser m b ()
asteriskP = void $ tokenP S.Asterisk

-- | Text
actualTextP :: IDocParser m b Text
actualTextP = unText <$> anyTokenP MP.<?> "Some Actual Text"
  where
    unText (S.TextT x) = x
    unText x = error $ "Not a text: `" ++ (show x) ++ "'."

textP :: IDocParser m b Text
textP = S.unToken <$> anyTokenP

mkQTextP :: S.Token -> S.TextType -> IDocParser m b (S.QText m)
mkQTextP t tt = MP.label "Quoted Text" $ do
  txt <- someTween tP tP simpleCoreP
  return $ S.QText { S._qtText = txt
                   , S._qtType = tt
                   }
  where tP = tokenP t

strongP :: IDocParser m b (S.QText m)
strongP = mkQTextP S.Asterisk S.Strong

emphasisP :: IDocParser m b (S.QText m)
emphasisP = mkQTextP S.Underscore S.Emphasis

monospaceP :: IDocParser m b (S.QText m)
monospaceP = mkQTextP S.BackTick S.Monospace

superscriptP :: IDocParser m b (S.QText m)
superscriptP = mkQTextP S.Caret S.Superscript

subscriptP :: IDocParser m b (S.QText m)
subscriptP = mkQTextP S.Tilde S.Subscript

qTextP :: IDocParser m b (S.QText m)
qTextP =  MP.try strongP
      <|> MP.try emphasisP
      <|> MP.try monospaceP
      <|> MP.try superscriptP
      <|>        subscriptP

-- | AttrMaps
attrValPairP :: IDocParser m b (S.AttrName, Maybe S.AttrValue)
attrValPairP = MP.label "Attribute-Value Pair" $ do
  n <- S.AttrName <$> actualTextP
  v <- optional $ do
    equalsP
    S.AttrValue <$> actualTextP
  return (n, v)

attrMapP :: IDocParser m b S.AttrMap
attrMapP = MP.label "An AttrMap" $ do
  lBracketP
  avps <- MP.sepBy1 attrValPairP commaP
  let am = S.AttrMap $ M.fromList avps
  rBracketP
  return am
  where
    lBracketP = void $ tokenP S.LBracket
    rBracketP = void $ tokenP S.RBracket
    commaP = void $ tokenP S.Comma

optionalAttrMapP :: IDocParser m b S.AttrMap
optionalAttrMapP = MP.label "An Optional AttrMap" $ do
  am <- optional attrMapP
  case am of
    Nothing -> return $ S.AttrMap M.empty
    Just am' -> return am'

-- | SetID
setIDP :: IDocParser m b (S.SetID m)
setIDP = MP.label "A SetID" $ do
  lBracketP >> lBracketP
  h <- idHashP
  rBracketP >> rBracketP
  return S.SetID { S._sidName = h }
  where
    lBracketP = void $ tokenP S.LBracket
    rBracketP = void $ tokenP S.RBracket

-- | Links
linkOpenerP :: IDocParser m b ()
linkOpenerP = do
  void langle
  void langle
  where
    langle = tokenP S.LAngle

linkCloserP :: IDocParser m b ()
linkCloserP = do
  void rangle
  void rangle
  where
    rangle = tokenP S.RAngle

protocolP :: IDocParser m b S.Protocol
protocolP = do
  x <- Text.IDoc.Parse.satisfy isRecognized
  colonP
  fSlashP
  return $ S.Protocol $ S.unToken x
  where
    isRecognized (S.TextT x) | x == "https" = True
                             | x == "http"  = True
                             | x == "youtube" = True
                             | x == "image" = True
                             | x == "audio" = True
                             | otherwise = False
    isRecognized _ = False
    colonP = void $ tokenP S.Colon

idBaseP :: IDocParser m b S.IDBase
idBaseP = S.IDBase <$> do
  fSlashP
  concat <$> (manyV $ do
                 MP.notFollowedBy $ MP.eitherP (MP.try fSlashP) $
                   MP.eitherP (MP.try rAngleP) $
                   MP.eitherP (MP.try octothorpeP) lBraceP
                 textP)
  where
    rAngleP = void $ tokenP S.RAngle
    lBraceP = void $ tokenP S.LBrace

idHashP :: IDocParser m b S.IDHash
idHashP = S.IDHash <$> do
  octothorpeP
  concat <$> (manyV $ do
                 MP.notFollowedBy $ MP.eitherP (MP.eitherP (MP.try rBracketP) rAngleP) rBraceP
                 textP)
  where
    rBracketP = void $ tokenP S.RBracket
    rAngleP   = void $ tokenP S.RAngle
    rBraceP   = void $ tokenP S.RBrace

idP :: IDocParser m b S.ID
idP = do
  p <- optional protocolP
  b <- manyV idBaseP
  h <- optional idHashP
  return S.ID { S._idProtocol = p
              , S._idBase = b
              , S._idHash = h
              }

linkP :: IDocParser m b (S.Link m)
linkP = MP.label "A Link" $ do
  r <- view rel
  ir <- view imgRel
  ar <- view audioRel
  am <- optionalAttrMapP
  linkOpenerP
  i <- idP
  case i of
    S.ID { S._idBase = b
         , S._idHash = Nothing
         , S._idProtocol = Nothing } | null b -> fail "Empty link."
    _ -> return ()
  let ty = case i of
        S.ID { S._idProtocol = Just (S.Protocol "image") } -> S.Out (Just ir)
        S.ID { S._idProtocol = Just (S.Protocol "audio") } -> S.Out (Just ar)
        S.ID { S._idProtocol = Just _ } -> S.Out Nothing
        S.ID { S._idBase = b } | null b -> S.Internal
        _                               -> S.Back r
  linkCloserP
  txt <- optionalSimpleMarkupContentsP
  return S.Link { S._linkText = if null txt then Nothing else Just $ S.LinkText txt
                , S._linkAttrs = am
                , S._linkLocation = i
                , S._linkType = ty
                }

-- | Inline Math
inlineMathP :: IDocParser m b (S.InlineMath m)
inlineMathP = MP.label "Some Inline Math" $ do
  am <- optionalAttrMapP
  cnt <- someTween dollarSignP dollarSignP anyTokenP
  sid <- optional setIDP
  return $ S.InlineMath { S._imAttrs = am
                        , S._imContents = cnt
                        , S._imSetID = sid
                        }
  where
    dollarSignP = void $ tokenP S.DollarSign

-- | Markup
markupContentsP :: IDocParser m b (Vector (S.SimpleCore m))
markupContentsP = MP.label "Some Text Between Braces" $ do
  someTween lBraceP rBraceP simpleCoreP
  where
    lBraceP = tokenP S.LBrace
    rBraceP = tokenP S.RBrace

markupIDHashP :: IDocParser m b S.IDHash
markupIDHashP = do
  void $ lBraceP
  s <- idHashP
  void $ rBraceP
  return s
  where
    lBraceP = tokenP S.LBrace
    rBraceP = tokenP S.RBrace

markupContentsWithSetIDP :: IDocParser m b ((Vector (S.SimpleCore m)), S.SetID m)
markupContentsWithSetIDP = do
  v <- markupContentsP
  s <- setIDP
  return (v, s)

optionalSimpleMarkupContentsP :: IDocParser m b Text
optionalSimpleMarkupContentsP = do
  mu <- optional $ someTween lBraceP rBraceP anyTokenP
  return $ maybe mempty (concatMap S.unToken) mu
  where
    lBraceP = tokenP S.LBrace
    rBraceP = tokenP S.RBrace

optionalMarkupContentsP :: IDocParser m b (Vector (S.SimpleCore m))
optionalMarkupContentsP = do
  mu <- optional markupContentsP
  return $ maybe mempty id mu

defaultMarkupP :: IDocParser m b (S.Markup m)
defaultMarkupP = do
  m <- view mp
  am <- optionalAttrMapP
  percentSignP
  name <- textP
  cnt <- m name
  sid <- optional setIDP
  return $ S.Markup { S._muType = cnt
                    , S._muAttrs = am
                    , S._muSetID = sid }
  where
    percentSignP = void $ tokenP S.PercentSign

-- citationP :: IDocParser m b S.Markup
-- citationP = mkMarkupP "cite" S.Citation MP.<?> "A Citation"

-- footnoteP :: IDocParser m b S.Markup
-- footnoteP = MP.label "A Footnote" $ do
--   mu <- mkMarkupP "footnote" S.Footnote 
--   sid <- setIDP
--   return $ mu { S._muSetID = Just sid }

-- footnoteRefP :: IDocParser m b S.Markup
-- footnoteRefP = mkMarkupP "footenoteref" S.FootnoteRef MP.<?> "A Footnote Reference"

-- markupP :: IDocParser m b S.Markup
-- markupP = MP.label "Some Markup" $ MP.try citationP
--                             <|> MP.try footnoteP
--                             <|>        footnoteRefP

-- | Paragraphs
paragraphP :: IDocParser m b (S.Paragraph m)
paragraphP = do
  void $ many newlineP
  cnt <- someV $ do
    MP.notFollowedBy $ MP.eitherP (MP.try $ newlineP >> some newlineP) $
                       MP.eitherP (MP.try $ blockEnderP') $
                                            doubleLBracketP
    simpleCoreP
  sid <- optional setIDP
  void $ many newlineP
  return $ S.Paragraph { S._paraContents = cnt
                       , S._paraSetID = sid
                       }
  where
    lBracketP = void $ tokenP S.LBracket
    doubleLBracketP = lBracketP >> lBracketP

blockTypeName :: IDocParser m b Text
blockTypeName = MP.label "A Block Type Name" $ do
  atSignP
  x <- textP
  newlineP
  return x
  where
    atSignP = void $ tokenP S.AtSign

optionalBlockTitle :: IDocParser m b (Maybe (S.BlockTitle m))
optionalBlockTitle = optional $ fmap S.BlockTitle $ MP.label "A Block Title" $ do
  octothorpeP
  someTill newlineP simpleCoreP

-- | Blocks
defaultBlockP :: IDocParser m b (S.Block m b)
defaultBlockP = MP.label "A Block" $ do
  b <- view bp
  am <- optionalAttrMapP
  void $ optional newlineP
  tyName <- blockTypeName
  title <- optionalBlockTitle
  ty <- b tyName
  sid <- optional setIDP
  return $ S.Block { S._bType = ty
                   , S._bAttrs = am
                   , S._bTitle = title
                   , S._bSetID = sid
                   }

blockStarterP :: IDocParser m b ()
blockStarterP = do
  dashP >> dashP >> dashP >> newlineP

blockEnderP :: IDocParser m b ()
blockEnderP = do
  dashP >> dashP >> dashP >> newlineP

blockContinuerP :: IDocParser m b ()
blockContinuerP = do
  plusP >> plusP >> plusP >> newlineP

blockEnderP' :: IDocParser m b ()
blockEnderP' = newlineP >> blockEnderP

multipartSeparatorP :: IDocParser m b ()
multipartSeparatorP = do
  asteriskP >> asteriskP >> asteriskP >> newlineP

simpleCoreBlockP :: IDocParser m b (Vector (S.SimpleCore m))
simpleCoreBlockP = do
  blockStarterP
  someTill blockEnderP' simpleCoreP

coreBlockP :: IDocParser m b (Vector (S.Core m b))
coreBlockP = do
  blockStarterP
  someTill (many newlineP >> blockEnderP) coreP

vectorLinkCoreBlockP :: IDocParser m b (Vector (S.Link m), Vector (S.Core m b))
vectorLinkCoreBlockP = do
  blockStarterP
  ls <- someTill (many newlineP >> blockContinuerP) $ do
    l <- linkP
    newlineP
    return l
  x <- someTill (many newlineP >> blockEnderP) coreP
  return (ls, x)

doubleCoreBlockP :: IDocParser m b (Vector (S.Core m b), Vector (S.Core m b))
doubleCoreBlockP = do
  blockStarterP
  x <- someTill (many newlineP >> blockContinuerP) coreP
  y <- someTill (many newlineP >> blockEnderP) coreP
  return (x, y)

coreBlockWithOptionalP :: IDocParser m b ( Vector (S.Core m b)
                                         , Maybe (Vector (S.Core m b)) )
coreBlockWithOptionalP = do
  blockStarterP
  (xs, e) <- someTill'
             (many newlineP >>
              (MP.eitherP (MP.try blockContinuerP) blockEnderP)) coreP
  op_ <- case e of
    Left _ -> Just <$> someTill (many newlineP >> blockEnderP) coreP
    Right _ -> return Nothing
  return (xs, op_)

uninterpretedBlockP :: IDocParser m b (Vector S.Token)
uninterpretedBlockP = do
  blockStarterP
  xs <- someV $ do
    MP.notFollowedBy blockEnderP'
    anyTokenP
  blockEnderP'
  return xs

linkBlockP :: IDocParser m b (S.Link m)
linkBlockP = do
  blockStarterP
  x <- linkP
  blockEnderP'
  return x

linkBlockWithOptionalP :: IDocParser m b (S.Link m, Maybe (Vector (S.SimpleCore m)))
linkBlockWithOptionalP = do
  blockStarterP
  x <- linkP
  newlineP
  e <- MP.eitherP (MP.try blockContinuerP) blockEnderP
  op_ <- case e of
    Left _ -> Just <$> someTill blockEnderP' simpleCoreP
    Right _ -> return Nothing
  return (x, op_)

-- | Section

sectionTitleP = do
  equalses <- some equalsP
  let ty = case length equalses of
             2 -> S.TopSection
             3 -> S.SubSection
             _ -> error "only `Section's and `Subsection's are allowed."
  title <- simpleLineP


sectionP :: IDocParser m b (S.Section m b)
sectionP = do
  am <- optionalAttrMapP
  sid <- optional setIDP
  void $ many newlineP
  cnt <- manyV $ do
    MP.notFollowedBy $ many newlineP >> equalsP >> equalsP
    x <- coreP
    void $ many newlineP
    return x
  return $ S.Section { S._secType = ty
                     , S._secAttrs = am
                     , S._secContents = cnt
                     , S._secTitle = S.SectionTitle title
                     , S._secSetID = sid
                     }
  where
    simpleLineP = someTill newlineP simpleCoreP

escapedP :: IDocParser m b Text
escapedP = do
  bSlashP
  textP
  where
    bSlashP = void $ tokenP S.BSlash

commentP :: IDocParser m b (Vector S.Token)
commentP = do
  fSlashP >> fSlashP
  someTill newlineP anyTokenP

-- | Core
-- Putting it all together.
simpleCoreP :: IDocParser m b (S.SimpleCore m)
simpleCoreP =  S.TextC       <$> MP.try escapedP
           <|> S.MarkupC     <$> MP.try defaultMarkupP
           <|> S.InlineMathC <$> MP.try inlineMathP
           <|> S.LinkC       <$> MP.try linkP
           <|> S.QTextC      <$> MP.try qTextP
           <|> S.CommentC    <$> MP.try commentP
           <|> S.TextC       <$>        textP

coreP :: IDocParser m b (S.Core m b)
coreP = S.CC <$> ((S.BlockC     <$> (MP.try defaultBlockP))
             <|>  (S.ParagraphC <$>         paragraphP))


equalsP :: IDocParser m b ()
equalsP = void $ tokenP S.Equals

docTitleP :: IDocParser m b (Vector (S.SimpleCore m))
docTitleP = someTween equalsP newlineP simpleCoreP

docP :: IDocParser m b (S.Doc m b)
docP = do
  title <- docTitleP
  docSid <- optional setIDP
  void $ many newlineP
  preamble <- manyV $ do
    MP.notFollowedBy $ many newlineP >> equalsP >> equalsP
    x <- coreP
    return x
  void $ many newlineP
  sections <- sepEndByV sectionP (many newlineP)
  let preambleSection = S.Section { S._secType = S.Preamble
                                  , S._secAttrs = S.AttrMap M.empty
                                  , S._secContents = preamble
                                  , S._secTitle = S.SectionTitle empty
                                  , S._secSetID = Nothing
                                  }
  return $ S.Doc { S._docTitle = S.DocTitle title
                 , S._docSections = preambleSection `V.cons` sections
                 , S._docSetID = docSid
                 }

uninterpret :: Vector S.Token -> Text
uninterpret = concatMap S.unToken

errorDoc :: Show e => e -> S.Doc m b
errorDoc e = S.Doc { S._docTitle = S.DocTitle $ singleton $ S.TextC "Error!"
                   , S._docSections = singleton $ S.Section { S._secAttrs = S.AttrMap mempty
                                                            , S._secContents = singleton $ S.CC $ S.ParagraphC $ S.Paragraph { _paraContents = singleton $ S.TextC $ fromString $ show e
                                                                                                                             , _paraSetID = Nothing }
                                                      , _secTitle = S.SectionTitle empty
                                                      , _secSetID = Nothing
                                                      , _secType = S.TopSection
                                                      }
                   , _docSetID = Nothing
                   }

compileIdoc :: MarkupParser m b -> BlockParser m b -> Text -> Text -> Text -> Text -> S.Doc m b
compileIdoc m b rel_ imgRel_ audioRel_ text_ = case MP.parse L.dTokens "<idoc>" text_ of
  Left e -> errorDoc e
  Right x -> do
    case runIdentity $ runReaderT (MP.runParserT (unIDocParser docP) "<idoc tokens>" x) (IDocReadState rel_ imgRel_ audioRel_ m b) of
      Left e -> errorDoc e
      Right y -> y

compileIdoc' :: MarkupParser m b -> BlockParser m b -> Text -> Text -> Text -> Text
             -> Either (MP.ParseErrorBundle Text Void)
                       (Either (MP.ParseErrorBundle S.IDocTokenStream Void)
                               (S.Doc m b))
compileIdoc' m b rel_ imgRel_ audioRel_ text_ = case MP.parse L.dTokens "<idoc>" text_ of
  Left eBundle -> Left eBundle
  Right x -> return $ runIdentity $ runReaderT (MP.runParserT (unIDocParser docP) "<idoc tokens>" x) (IDocReadState rel_ imgRel_ audioRel_ m b)

parseIdoc' :: IDocParser m b p -> MarkupParser m b -> BlockParser m b -> Text -> Text -> Text -> Text
             -> Either (MP.ParseErrorBundle Text Void)
                       (Either (MP.ParseErrorBundle S.IDocTokenStream Void)
                               p)
parseIdoc' p m b rel_ imgRel_ audioRel_ text_ = case MP.parse L.dTokens "<idoc>" text_ of
  Left e -> Left e
  Right x -> return $ runIdentity $ runReaderT (MP.runParserT (unIDocParser p) "<idoc tokens>" x) (IDocReadState rel_ imgRel_ audioRel_ m b)
