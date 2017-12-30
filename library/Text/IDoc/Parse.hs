-- | Parse2.hs
-- Author: Matt Walker
-- License: https://opensource.org/licenses/BSD-2-Clause
-- Created: Aug 25, 2017
-- Summary: 
module Text.IDoc.Parse where

import ClassyPrelude as CP

import qualified Data.List.NonEmpty as NE
import qualified Data.Map as M
import qualified Data.Vector as V
import Data.Void

import qualified Text.Megaparsec as MP

import qualified Text.IDoc.Syntax as S
import qualified Text.IDoc.Lex as L

type IDocParseError = MP.ParseError S.DToken (MP.ErrorFancy Void)
type IDocParser = MP.Parsec (MP.ErrorFancy Void) S.IDocTokenStream

type BlockTypeName = Text
type InnerBlockParser m b = MarkupParser m -> BlockTypeName -> IDocParser (b m)
type BlockParser m b = MarkupParser m -> BlockTypeName -> IDocParser (b m)

type MarkupTypeName = Text
type MarkupParser m = MarkupTypeName -> IDocParser m

manyV :: IDocParser a -> IDocParser (Vector a)
manyV x = fromList <$> many x

someV :: IDocParser a -> IDocParser (Vector a)
someV x = fromList <$> some x

sepEndByV :: IDocParser a -> IDocParser sep -> IDocParser (Vector a)
sepEndByV x sep = fromList <$> (MP.sepEndBy x sep)

sepEndBy1V :: IDocParser a -> IDocParser sep -> IDocParser (Vector a)
sepEndBy1V x sep = fromList <$> (MP.sepEndBy1 x sep)

sepByV :: IDocParser a -> IDocParser sep -> IDocParser (Vector a)
sepByV x sep = fromList <$> (MP.sepBy x sep)

sepBy1V :: IDocParser a -> IDocParser sep -> IDocParser (Vector a)
sepBy1V x sep = fromList <$> (MP.sepBy1 x sep)

manyTween :: IDocParser l -> IDocParser r -> IDocParser a -> IDocParser (Vector a)
manyTween l r x = do
  void l
  xs <- manyV $ do
    MP.notFollowedBy r
    x
  void r
  return xs

someTween :: IDocParser l -> IDocParser r -> IDocParser a -> IDocParser (Vector a)
someTween l r x = do
  void l
  xs <- someV $ do
    MP.notFollowedBy r
    x
  void r
  return xs

someTill :: IDocParser ender -> IDocParser a -> IDocParser (Vector a)
someTill e x = do
  xs <- someV $ do
    MP.notFollowedBy e
    x
  void e
  return xs

someTill' :: IDocParser ender -> IDocParser a -> IDocParser (Vector a, ender)
someTill' e x = do
  xs <- someV $ do
    MP.notFollowedBy e
    x
  e_ <- e
  return (xs, e_)

manyTill :: IDocParser ender -> IDocParser a -> IDocParser (Vector a)
manyTill e x = do
  xs <- manyV $ do
    MP.notFollowedBy e
    x
  void e
  return xs

satisfy :: (MP.MonadParsec e s m, MP.Token s ~ S.DToken) => (S.Token -> Bool) -> m S.Token
satisfy f = MP.token test Nothing
  where
    test (y@S.DebugToken { S._dtToken = x }) =
      if f x then
        Right x
      else
        Left (pure (MP.Tokens (y NE.:| [])), mempty)

tokenP :: S.Token -> IDocParser S.Token
tokenP t = Text.IDoc.Parse.satisfy (== t) MP.<?> (show t)

anyTokenP :: IDocParser S.Token
anyTokenP = Text.IDoc.Parse.satisfy (const True) MP.<?> "Any Token"

newlineP :: IDocParser ()
newlineP = void $ tokenP S.Newline

dashP :: IDocParser ()
dashP = void $ tokenP S.Dash

plusP :: IDocParser ()
plusP = void $ tokenP S.Plus

fSlashP :: IDocParser ()
fSlashP = void $ tokenP S.FSlash

octothorpeP :: IDocParser ()
octothorpeP = void $ tokenP S.Octothorpe

asteriskP :: IDocParser ()
asteriskP = void $ tokenP S.Asterisk

-- | Text
actualTextP :: IDocParser Text
actualTextP = unText <$> anyTokenP MP.<?> "Some Actual Text"
  where
    unText (S.TextT x) = x
    unText x = error $ "Not a text: `" ++ (show x) ++ "'."

textP :: IDocParser Text
textP = S.unToken <$> anyTokenP

mkQTextP :: MarkupParser m -> S.Token -> S.TextType -> IDocParser (S.QText m)
mkQTextP m t tt = MP.label "Quoted Text" $ do
  txt <- someTween tP tP (simpleCoreP m)
  return $ S.QText { S._qtText = txt
                   , S._qtType = tt
                   }
  where tP = tokenP t

strongP :: MarkupParser m -> IDocParser (S.QText m)
strongP m = mkQTextP m S.Asterisk S.Strong
  
emphasisP :: MarkupParser m -> IDocParser (S.QText m)
emphasisP m = mkQTextP m S.Underscore S.Emphasis

monospaceP :: MarkupParser m -> IDocParser (S.QText m)
monospaceP m = mkQTextP m S.BackTick S.Monospace

superscriptP :: MarkupParser m -> IDocParser (S.QText m)
superscriptP m = mkQTextP m S.Caret S.Superscript

subscriptP :: MarkupParser m -> IDocParser (S.QText m)
subscriptP m = mkQTextP m S.Tilde S.Subscript

qTextP :: MarkupParser m -> IDocParser (S.QText m)
qTextP m =  MP.try (strongP m)
        <|> MP.try (emphasisP m)
        <|> MP.try (monospaceP m)
        <|> MP.try (superscriptP m)
        <|>        (subscriptP m)

-- | AttrMaps
attrValPairP :: IDocParser (S.AttrName, Maybe S.AttrValue)
attrValPairP = MP.label "Attribute-Value Pair" $ do
  n <- S.AttrName <$> actualTextP
  v <- optional $ do
    equalsP
    S.AttrValue <$> actualTextP
  return (n, v)
  where
    equalsP = void $ tokenP S.Equals

attrMapP :: IDocParser S.AttrMap
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

optionalAttrMapP :: IDocParser S.AttrMap
optionalAttrMapP = MP.label "An Optional AttrMap" $ do
  am <- optional attrMapP
  case am of
    Nothing -> return $ S.AttrMap M.empty
    Just am' -> return $ am'

-- | SetID
setIDP :: MarkupParser m -> IDocParser (S.SetID m)
setIDP m = MP.label "A SetID" $ do
  lBracketP >> lBracketP
  h <- idHashP
  rBracketP >> rBracketP
  d <- markupContentsP m
  return $ S.SetID { S._sidName = h
                   , S._sidDisplay = d }
  where
    lBracketP = void $ tokenP S.LBracket
    rBracketP = void $ tokenP S.RBracket

-- | Links
linkOpenerP :: IDocParser ()
linkOpenerP = do
  void $ langle
  void $ langle
  where
    langle = tokenP S.LAngle

linkCloserP :: IDocParser ()
linkCloserP = do
  void $ rangle
  void $ rangle
  where
    rangle = tokenP S.RAngle

protocolP :: IDocParser S.Protocol
protocolP = do
  x <- Text.IDoc.Parse.satisfy isRecognized
  colonP
  fSlashP
  return $ S.Protocol $ S.unToken x
  where
    isRecognized (S.TextT x) | x == "https" = True
                             | x == "http"  = True
                             | x == "youtube" = True
                             | otherwise = False
    isRecognized _ = False
    colonP = void $ tokenP S.Colon

idBaseP :: IDocParser S.IDBase
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

idHashP :: IDocParser S.IDHash
idHashP = S.IDHash <$> do
  octothorpeP
  concat <$> (manyV $ do
                 MP.notFollowedBy $ MP.eitherP (MP.try rBracketP) rAngleP
                 textP)
  where
    rBracketP = void $ tokenP S.RBracket
    rAngleP = void $ tokenP S.RAngle

idP :: IDocParser S.ID
idP = do
  p <- optional protocolP
  b <- manyV idBaseP
  h <- optional idHashP
  return $ S.ID { S._idProtocol = p
                , S._idBase = b
                , S._idHash = h
                }
  where

linkP :: MarkupParser m -> IDocParser (S.Link m)
linkP m = MP.label "A Link" $ do
  am <- optionalAttrMapP
  linkOpenerP
  i <- idP
  let ty = case i of
        S.ID { S._idProtocol = Just _ } -> S.Out
        S.ID { S._idBase = b } | null b -> S.Internal
        _                               -> S.Back
  linkCloserP
  txt <- optionalMarkupContentsP m
  return $ S.Link { S._linkText = S.LinkText txt
                  , S._linkAttrs = am
                  , S._linkLocation = i
                  , S._linkType = ty
                  }

-- | Lists
mkListItemP :: MarkupParser m -> S.Token -> Bool -> S.ListType-> IDocParser (S.ListItem m)
mkListItemP m starter hasLabel ty = do
  starterP
  lbl <- if hasLabel then
           Just <$> (someTill doubleStarterP (simpleCoreP m))
         else
           return Nothing
  cnt <- someV $ do
    MP.notFollowedBy $ newlineP >> some newlineP
    simpleCoreP m
  return $ S.ListItem { S._liAttrs = S.AttrMap M.empty
                      , S._liLabel = S.ListLabel <$> lbl
                      , S._liContents = cnt
                      , S._liSetID = Nothing
                      -- FIXME: This should not be Nothing!
                      , S._liType = ty
                      }
  where
    starterP = void $ tokenP starter
    doubleStarterP = starterP >> starterP

unorderedItemP :: MarkupParser m -> IDocParser (S.ListItem m)
unorderedItemP m = mkListItemP m S.Dash False S.Unordered MP.<?> "An Unordered List Item"

orderedItemP :: MarkupParser m -> IDocParser (S.ListItem m)
orderedItemP m = mkListItemP m S.Period False S.Ordered MP.<?> "An Ordered List Item"

labelledItemP :: MarkupParser m -> IDocParser (S.ListItem m)
labelledItemP m = mkListItemP m S.Colon True S.Labelled MP.<?> "A Labelled List Item"

listP :: MarkupParser m -> IDocParser (S.List m)
listP m = MP.label "A List" $ S.List <$> (MP.try (sepEndBy1V (unorderedItemP m) (some newlineP))
                                     <|>  MP.try (sepEndBy1V (orderedItemP m)   (some newlineP))
                                     <|>         (sepEndBy1V (labelledItemP m)  (some newlineP)))

-- | Inline Math
inlineMathP :: MarkupParser m -> IDocParser (S.InlineMath m)
inlineMathP m = MP.label "Some Inline Math" $ do
  am <- optionalAttrMapP
  cnt <- someTween dollarSignP dollarSignP anyTokenP
  sid <- optional (setIDP m)
  return $ S.InlineMath { S._imAttrs = am
                        , S._imContents = cnt
                        , S._imSetID = sid
                        }
  where
    dollarSignP = void $ tokenP S.DollarSign

-- | Markup
markupContentsP :: MarkupParser m -> IDocParser (Vector (S.SimpleCore m))
markupContentsP m = MP.label "Some Text Between Braces" $ do
  someTween lBraceP rBraceP (simpleCoreP m)
  where
    lBraceP = tokenP S.LBrace
    rBraceP = tokenP S.RBrace

optionalMarkupContentsP :: MarkupParser m -> IDocParser (Vector (S.SimpleCore m))
optionalMarkupContentsP m = do
  mu <- optional (markupContentsP m)
  case mu of
    Nothing -> return empty
    Just xs -> return xs

defaultMarkupP :: MarkupParser m -> IDocParser (S.Markup m)
defaultMarkupP m = do
  am <- optionalAttrMapP
  percentSignP
  name <- textP
  percentSignP
  cnt <- m name
  sid <- optional (setIDP m)
  return $ S.Markup { S._muType = cnt
                    , S._muAttrs = am
                    , S._muSetID = sid
                    }
  where
    percentSignP = void $ tokenP S.PercentSign

-- citationP :: IDocParser S.Markup
-- citationP = mkMarkupP "cite" S.Citation MP.<?> "A Citation"

-- footnoteP :: IDocParser S.Markup
-- footnoteP = MP.label "A Footnote" $ do
--   mu <- mkMarkupP "footnote" S.Footnote 
--   sid <- setIDP
--   return $ mu { S._muSetID = Just sid }

-- footnoteRefP :: IDocParser S.Markup
-- footnoteRefP = mkMarkupP "footenoteref" S.FootnoteRef MP.<?> "A Footnote Reference"

-- markupP :: IDocParser S.Markup
-- markupP = MP.label "Some Markup" $ MP.try citationP
--                             <|> MP.try footnoteP
--                             <|>        footnoteRefP

-- | Paragraphs
paragraphP :: MarkupParser m -> IDocParser (S.Paragraph m)
paragraphP m = do
  void $ many newlineP
  cnt <- someV $ do
    MP.notFollowedBy $ MP.eitherP (MP.try $ newlineP >> some newlineP) $
                       MP.eitherP (MP.try $ blockEnderP') $
                                            doubleLBracketP
    simpleCoreP m
  sid <- optional (setIDP m)
  void $ many newlineP
  return $ S.Paragraph { S._paraContents = cnt
                       , S._paraSetID = sid
                       }
  where 
    lBracketP = void $ tokenP S.LBracket
    doubleLBracketP = lBracketP >> lBracketP

blockTypeName :: IDocParser Text
blockTypeName = MP.label "A Block Type Name" $ do
  atSignP
  x <- textP
  newlineP
  return x
  where  
    atSignP = void $ tokenP S.AtSign

optionalBlockTitle :: MarkupParser m -> IDocParser (Maybe (S.BlockTitle m))
optionalBlockTitle m = optional $ fmap S.BlockTitle $ MP.label "A Block Title" $ do
  octothorpeP
  someTill newlineP (simpleCoreP m)

-- | Blocks
defaultBlockP :: MarkupParser m -> InnerBlockParser m b -> IDocParser (S.Block m b)
defaultBlockP m b = MP.label "A Block" $ do
  am <- optionalAttrMapP
  void $ optional newlineP
  tyName <- blockTypeName
  title <- optionalBlockTitle m
  ty <- b m tyName
  sid <- optional (setIDP m)
  return $ S.Block { S._bType = ty
                   , S._bAttrs = am
                   , S._bTitle = title
                   , S._bSetID = sid
                   }

blockStarterP :: IDocParser ()
blockStarterP = do
  dashP >> dashP >> dashP >> newlineP

blockEnderP :: IDocParser ()
blockEnderP = do
  dashP >> dashP >> dashP >> newlineP

blockContinuerP :: IDocParser ()
blockContinuerP = do
  plusP >> plusP >> plusP >> newlineP

blockEnderP' :: IDocParser ()
blockEnderP' = newlineP >> blockEnderP

multipartSeparatorP :: IDocParser ()
multipartSeparatorP = do
  asteriskP >> asteriskP >> asteriskP >> newlineP

simpleCoreBlockP :: MarkupParser m -> IDocParser (Vector (S.SimpleCore m))
simpleCoreBlockP m = do
  blockStarterP
  someTill blockEnderP' (simpleCoreP m)

coreBlockP :: MarkupParser m -> BlockParser m b -> IDocParser (Vector (S.Core m b))
coreBlockP m b = do
  blockStarterP
  someTill (many newlineP >> blockEnderP) (coreP m b)

vectorLinkCoreBlockP :: MarkupParser m -> BlockParser m b
                     -> IDocParser (Vector (S.Link m), Vector (S.Core m b))
vectorLinkCoreBlockP m b = do
  blockStarterP
  ls <- someTill (many newlineP >> blockContinuerP) $ do
    l <- (linkP m)
    newlineP
    return l
  x <- someTill (many newlineP >> blockEnderP) (coreP m b)
  return (ls, x)

doubleCoreBlockP :: MarkupParser m -> BlockParser m b
                 -> IDocParser (Vector (S.Core m b), Vector (S.Core m b))
doubleCoreBlockP m b = do
  blockStarterP
  x <- someTill (many newlineP >> blockContinuerP) (coreP m b)
  y <- someTill (many newlineP >> blockEnderP) (coreP m b)
  return (x, y)

coreBlockWithOptionalP :: MarkupParser m -> BlockParser m b
                       -> IDocParser ( Vector (S.Core m b)
                                     , Maybe (Vector (S.Core m b)) )
coreBlockWithOptionalP m b = do
  blockStarterP
  (xs, e) <- someTill' 
             (many newlineP >> 
               (MP.eitherP (MP.try blockContinuerP) blockEnderP)) (coreP m b)
  op <- case e of
          Left _ -> Just <$> (someTill (many newlineP >> blockEnderP) (coreP m b))
          Right _ -> return Nothing
  return (xs, op)

uninterpretedBlockP :: IDocParser (Vector S.Token)
uninterpretedBlockP = do
  blockStarterP
  xs <- someV $ do
    MP.notFollowedBy blockEnderP'
    anyTokenP
  blockEnderP'
  return xs

linkBlockP :: MarkupParser m -> IDocParser (S.Link m)
linkBlockP m = do
  blockStarterP
  x <- linkP m
  blockEnderP'
  return x

linkBlockWithOptionalP :: MarkupParser m -> IDocParser (S.Link m, Maybe (Vector (S.SimpleCore m)))
linkBlockWithOptionalP m = do
  blockStarterP
  x <- linkP m
  newlineP
  e <- MP.eitherP (MP.try blockContinuerP) blockEnderP
  op <- case e of
    Left _ -> Just <$> (someTill blockEnderP' (simpleCoreP m))
    Right _ -> return Nothing
  return (x, op)

-- | Section
sectionP :: MarkupParser m -> BlockParser m b -> IDocParser (S.Section m b)
sectionP m b = do
  am <- optionalAttrMapP
  equalses <- some equalsP
  let ty = case length equalses of
             2 -> S.TopSection
             3 -> S.SubSection
             _ -> error "only `Section's and `Subsection's are allowed."
  title <- simpleLineP
  sid <- optional (setIDP m)
  void $ many newlineP
  cnt <- manyV $ do
    MP.notFollowedBy $ many newlineP >> equalsP >> equalsP
    x <- coreP m b
    void $ many newlineP
    return x
  return $ S.Section { S._secType = ty
                     , S._secAttrs = am
                     , S._secContents = cnt
                     , S._secTitle = S.SectionTitle title
                     , S._secSetID = sid
                     }
  where
    equalsP = void $ tokenP S.Equals
    simpleLineP = someTill newlineP (simpleCoreP m)

escapedP :: IDocParser Text
escapedP = do
  bSlashP
  textP
  where
    bSlashP = void $ tokenP S.BSlash

-- | Core
-- Putting it all together.
simpleCoreP :: MarkupParser m -> IDocParser (S.SimpleCore m)
simpleCoreP m =  S.TextC       <$> MP.try escapedP
             <|> S.MarkupC     <$> MP.try (defaultMarkupP m)
             <|> S.InlineMathC <$> MP.try (inlineMathP m)
             <|> S.LinkC       <$> MP.try (linkP m)
             <|> S.QTextC      <$> MP.try (qTextP m)
             <|> S.TextC       <$>        textP

coreP :: MarkupParser m -> BlockParser m b -> IDocParser (S.Core m b)
coreP m b = S.CC <$> ((S.BlockC     <$> (MP.try (defaultBlockP m b)))
                 <|>  (S.ListC      <$> (MP.try (listP m)))
                 <|>  (S.ParagraphC <$>         (paragraphP m)))

docP :: MarkupParser m -> BlockParser m b -> IDocParser (S.Doc m b)
docP m b = do
  title <- someTween equalsP newlineP (simpleCoreP m)
  docSid <- optional (setIDP m)
  void $ many newlineP
  preamble <- manyV $ do
    MP.notFollowedBy $ many newlineP >> equalsP >> equalsP
    x <- coreP m b
    return x
  void $ many newlineP
  sections <- sepEndByV (sectionP m b) (many newlineP)
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
  where
    equalsP = void $ tokenP S.Equals

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

compileIdoc :: Monad n => MarkupParser m -> BlockParser m b -> Text -> n (S.Doc m b)
compileIdoc m b text_ = case MP.parse L.dTokens "<idoc>" text_ of
                          Left e -> return $ errorDoc e
                          Right x -> do
                            case MP.parse (docP m b) "<idoc tokens>" x of
                              Left e -> return $ errorDoc e
                              Right y -> return y
