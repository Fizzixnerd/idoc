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
type InnerBlockParser a = BlockTypeName -> IDocParser a
type BlockParser a = BlockTypeName -> IDocParser a

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

mkQTextP :: S.Token -> S.TextType -> IDocParser S.QText
mkQTextP t tt = MP.label "Quoted Text" $ do
  txt <- someTween tP tP simpleCoreP
  return $ S.QText { S._qtText = txt
                   , S._qtType = tt
                   }
  where tP = tokenP t

strongP :: IDocParser S.QText
strongP = mkQTextP S.Asterisk S.Strong
  
emphasisP :: IDocParser S.QText
emphasisP = mkQTextP S.Underscore S.Emphasis

monospaceP :: IDocParser S.QText
monospaceP = mkQTextP S.BackTick S.Monospace

superscriptP :: IDocParser S.QText
superscriptP = mkQTextP S.Caret S.Superscript

subscriptP :: IDocParser S.QText
subscriptP = mkQTextP S.Tilde S.Subscript

qTextP :: IDocParser S.QText
qTextP =  MP.try strongP
      <|> MP.try emphasisP
      <|> MP.try monospaceP
      <|> MP.try superscriptP
      <|>        subscriptP

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
setIDP :: IDocParser S.SetID
setIDP = MP.label "A SetID" $ do
  lBracketP >> lBracketP
  h <- idHashP
  rBracketP >> rBracketP
  d <- markupContentsP
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

linkP :: IDocParser S.Link
linkP = MP.label "A Link" $ do
  am <- optionalAttrMapP
  linkOpenerP
  i <- idP
  let ty = case i of
        S.ID { S._idProtocol = Just _ } -> S.Out
        S.ID { S._idBase = b } | null b -> S.Internal
        _                               -> S.Back
  linkCloserP
  txt <- optionalMarkupContentsP
  return $ S.Link { S._linkText = S.LinkText txt
                  , S._linkAttrs = am
                  , S._linkLocation = i
                  , S._linkType = ty
                  }

-- | Lists
mkListItemP :: S.Token -> Bool -> S.ListType-> IDocParser S.ListItem
mkListItemP starter hasLabel ty = do
  starterP
  lbl <- if hasLabel then
           Just <$> (someTill doubleStarterP simpleCoreP)
         else
           return Nothing
  cnt <- someV $ do
    MP.notFollowedBy $ newlineP >> some newlineP
    simpleCoreP
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

unorderedItemP :: IDocParser S.ListItem
unorderedItemP = mkListItemP S.Dash False S.Unordered MP.<?> "An Unordered List Item"

orderedItemP :: IDocParser S.ListItem
orderedItemP = mkListItemP S.Period False S.Ordered MP.<?> "An Ordered List Item"

labelledItemP :: IDocParser S.ListItem
labelledItemP = mkListItemP S.Colon True S.Labelled MP.<?> "A Labelled List Item"

listP :: IDocParser S.List
listP = MP.label "A List" $ S.List <$> (     MP.try (sepEndBy1V unorderedItemP (some newlineP))
                                      <|> MP.try (sepEndBy1V orderedItemP   (some newlineP))
                                      <|>        (sepEndBy1V labelledItemP  (some newlineP)))

-- | Inline Math
inlineMathP :: IDocParser S.InlineMath 
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
markupContentsP :: IDocParser (Vector S.SimpleCore)
markupContentsP = MP.label "Some Text Between Braces" $ do
  someTween lBraceP rBraceP simpleCoreP
  where
    lBraceP = tokenP S.LBrace
    rBraceP = tokenP S.RBrace

optionalMarkupContentsP :: IDocParser (Vector S.SimpleCore)
optionalMarkupContentsP = do
  mu <- optional markupContentsP
  case mu of
    Nothing -> return empty
    Just xs -> return xs

mkMarkupP :: Text -> S.MarkupType -> IDocParser S.Markup
mkMarkupP name ty = do
  am <- optionalAttrMapP
  percentSignP
  void $ Text.IDoc.Parse.satisfy (== (S.TextT name))
  percentSignP
  cnt <- markupContentsP
  return $ S.Markup { S._muType = ty
                    , S._muAttrs = am
                    , S._muContents = cnt
                    , S._muSetID = Nothing
                    }
  where
    percentSignP = void $ tokenP S.PercentSign

citationP :: IDocParser S.Markup
citationP = mkMarkupP "cite" S.Citation MP.<?> "A Citation"

footnoteP :: IDocParser S.Markup
footnoteP = MP.label "A Footnote" $ do
  mu <- mkMarkupP "footnote" S.Footnote 
  sid <- setIDP
  return $ mu { S._muSetID = Just sid }

footnoteRefP :: IDocParser S.Markup
footnoteRefP = mkMarkupP "footenoteref" S.FootnoteRef MP.<?> "A Footnote Reference"

markupP :: IDocParser S.Markup
markupP = MP.label "Some Markup" $ MP.try citationP
                            <|> MP.try footnoteP
                            <|>        footnoteRefP

-- | Paragraphs
paragraphP :: IDocParser S.Paragraph
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

blockTypeName :: IDocParser Text
blockTypeName = MP.label "A Block Type Name" $ do
  atSignP
  x <- textP
  newlineP
  return x
  where  
    atSignP = void $ tokenP S.AtSign

optionalBlockTitle :: IDocParser (Maybe S.BlockTitle)
optionalBlockTitle = optional $ fmap S.BlockTitle $ MP.label "A Block Title" $ do
  octothorpeP
  someTill newlineP simpleCoreP

-- | Blocks
defaultBlockP :: InnerBlockParser a -> IDocParser (S.Block a)
defaultBlockP b = MP.label "A Block" $ do
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

simpleCoreBlockP :: IDocParser (Vector S.SimpleCore)
simpleCoreBlockP = do
  blockStarterP
  someTill blockEnderP' simpleCoreP

coreBlockP :: BlockParser a -> IDocParser (Vector (S.Core a))
coreBlockP b = do
  blockStarterP
  someTill (many newlineP >> blockEnderP) (coreP b)

vectorLinkCoreBlockP :: BlockParser a
                     -> IDocParser (Vector S.Link, Vector (S.Core a))
vectorLinkCoreBlockP b = do
  blockStarterP
  ls <- someTill (many newlineP >> blockContinuerP) $ do
    l <- linkP
    newlineP
    return l
  x <- someTill (many newlineP >> blockEnderP) (coreP b)
  return (ls, x)

doubleCoreBlockP :: BlockParser a
                 -> IDocParser (Vector (S.Core a), Vector (S.Core a))
doubleCoreBlockP b = do
  blockStarterP
  x <- someTill (many newlineP >> blockContinuerP) (coreP b)
  y <- someTill (many newlineP >> blockEnderP) (coreP b)
  return (x, y)

coreBlockWithOptionalP :: BlockParser a 
                       -> IDocParser ( Vector (S.Core a)
                                     , Maybe (Vector (S.Core a)) )
coreBlockWithOptionalP b = do
  blockStarterP
  (xs, e) <- someTill' 
             (many newlineP >> 
               (MP.eitherP (MP.try blockContinuerP) blockEnderP)) (coreP b)
  op <- case e of
          Left _ -> Just <$> (someTill (many newlineP >> blockEnderP) (coreP b))
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

linkBlockP :: IDocParser S.Link
linkBlockP = do
  blockStarterP
  x <- linkP
  blockEnderP'
  return x

linkBlockWithOptionalP :: IDocParser (S.Link, Maybe (Vector S.SimpleCore))
linkBlockWithOptionalP = do
  blockStarterP
  x <- linkP
  newlineP
  e <- MP.eitherP (MP.try blockContinuerP) blockEnderP
  op <- case e of
    Left _ -> Just <$> (someTill blockEnderP' simpleCoreP)
    Right _ -> return Nothing
  return (x, op)

-- | Section
sectionP :: BlockParser a -> IDocParser (S.Section a)
sectionP b = do
  am <- optionalAttrMapP
  equalses <- some equalsP
  let ty = case length equalses of
             2 -> S.TopSection
             3 -> S.SubSection
             _ -> error "only `Section's and `Subsection's are allowed."
  title <- simpleLineP
  sid <- optional setIDP
  void $ many newlineP
  cnt <- manyV $ do
    MP.notFollowedBy $ many newlineP >> equalsP >> equalsP
    x <- coreP b
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
    simpleLineP = someTill newlineP simpleCoreP

escapedP :: IDocParser Text
escapedP = do
  bSlashP
  textP
  where
    bSlashP = void $ tokenP S.BSlash

-- | Core
-- Putting it all together.
simpleCoreP :: IDocParser S.SimpleCore
simpleCoreP =  S.TextC       <$> MP.try escapedP
           <|> S.MarkupC     <$> MP.try markupP
           <|> S.InlineMathC <$> MP.try inlineMathP
           <|> S.LinkC       <$> MP.try linkP
           <|> S.QTextC      <$> MP.try qTextP
           <|> S.TextC       <$>        textP

coreP :: BlockParser a -> IDocParser (S.Core a)
coreP b = S.CC <$> ((S.BlockC     <$> (MP.try (defaultBlockP b)))
               <|>  (S.ListC      <$> (MP.try listP))
               <|>  (S.ParagraphC <$>         paragraphP))

docP :: BlockParser a -> IDocParser (S.Doc a)
docP b = do
  title <- someTween equalsP newlineP simpleCoreP
  docSid <- optional setIDP
  void $ many newlineP
  preamble <- manyV $ do
    MP.notFollowedBy $ many newlineP >> equalsP >> equalsP
    x <- coreP b
    return x
  void $ many newlineP
  sections <- sepEndByV (sectionP b) (many newlineP)
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

errorDoc :: Show e => e -> S.Doc a
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

compileIdoc :: Monad m => BlockParser a -> Text -> m (S.Doc a)
compileIdoc b text_ = case MP.parse L.dTokens "<idoc>" text_ of
                        Left e -> return $ errorDoc e
                        Right x -> do
                          case MP.parse (docP b) "<idoc tokens>" x of
                            Left e -> return $ errorDoc e
                            Right y -> return y
