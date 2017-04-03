-- | Parse.hs
-- Author: Matt Walker
-- License: https://opensource.org/licenses/BSD-2-Clause
-- Created: Mar 23, 2017
-- Summary: 

{-# LANGUAGE Arrows #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Text.IDoc.Parse where

import ClassyPrelude
import Text.Megaparsec as TM
import Data.Char

class IDoc e s m a where
  parseDoc :: ParsecT e s m a

newtype Opener a = Opener String deriving (Eq, Show, IsString)
newtype Closer a = Closer String deriving (Eq, Show, IsString)
class Between a where
  opener :: Opener a
  closer :: Closer a

class Contents e s m contents where
  data Container contents :: *
  contentsP :: ParsecT e s m contents
  wrapperP :: ParsecT e s m contents -> ParsecT e s m (Container contents)

newtype Sep a = Sep String deriving (Eq, Show, IsString)
class Separated a where
  sep :: Sep a

newtype TwinSep a b = TwinSep String deriving (Eq, Show, IsString)
class Twin a b where
  twinSep :: TwinSep a b

newtype Escape a = Escape Char deriving (Eq, Show)
class Escapable a where
  escape :: Vector (Escape a)

newtype LineStarter a = LineStarter String deriving (Eq, Show, IsString)
class Line a where
  lineStarter :: LineStarter a

class Textlike a where
  fromText :: Text -> a

newtype Markup a = Markup String deriving (Eq, Show, IsString)
class Markup1 a where
  markup1 :: Markup a

class Markup2 a where
  markup2 :: Markup a

class MarkupTuple a b | a -> b where
  fromMarkupTuple :: (AttrList, b, Maybe IDHash) -> a

paraDelim :: (ErrorComponent e, Stream s, Token s ~ Char) => ParsecT e s m ()
paraDelim = void $ string "\n\n" <|> string "\n+\n"

parseTwin :: forall e s m a b. ( ErrorComponent e
                               , Stream s
                               , Token s ~ Char
                               , IDoc e s m a
                               , IDoc e s m b
                               , Twin a b ) => ParsecT e s m (a, b)
parseTwin = do
  let (TwinSep ts :: TwinSep a b) = twinSep
  a <- parseDoc
  void $ string ts
  b <- parseDoc
  return (a, b)

parseEscapableTextBetween :: forall e s m a. ( ErrorComponent e
                                             , Stream s
                                             , Token s ~ Char
                                             , Escapable a
                                             , Textlike a
                                             , Between a) => ParsecT e s m a
parseEscapableTextBetween = do
  let (Closer cl :: Closer a) = closer
      (Opener op :: Opener a) = opener
      es = (\(Escape e) -> e) <$> (escape :: Vector (Escape a))
  void $ string op
  ret <- many $ do
    notFollowedBy $ string cl
    foldl' (<|>) (fail "This should never show --parseEscapableTextBetween.") $ fromList [ TM.try $ char '\\' >> char '\\' ] <> (fmap (\e -> TM.try $ char '\\' >> char e) es) <> fromList [ satisfy (\c -> isPrint c || isSpace c) ]
  void $ string cl
  return $ fromText $ fromString ret

parseSeparated :: forall e s m a. ( ErrorComponent e
                                  , Stream s
                                  , Token s ~ Char
                                  , IDoc e s m a
                                  , Separated a) => ParsecT e s m (Vector a)
parseSeparated = do
  let (Sep s :: Sep a) = sep
  ret <- sepBy1 parseDoc $ string s
  return $ fromList ret

parseContentsBetween :: forall e s m a. ( ErrorComponent e
                                        , Stream s
                                        , Token s ~ Char
                                        , Contents e s m a
                                        , Between a) => ParsecT e s m (Container a)
parseContentsBetween = do
  let (Closer cl :: Closer a) = closer
      (Opener op :: Opener a) = opener
  void $ string op
  ret <- wrapperP contentsP 
  void $ string cl
  return $ ret

parseLineText :: forall e s m a. ( ErrorComponent e
                                 , Stream s
                                 , Token s ~ Char
                                 , Line a
                                 , Textlike a) => ParsecT e s m a
parseLineText = do
  let (LineStarter ls :: LineStarter a) = lineStarter
  void $ string ls
  void $ space
  l <- many $ satisfy (\c -> isPrint c) -- automatically doesn't include '\n'
  void $ newline
  return $ fromText $ fromString l

parseMarkup1 :: forall e s m a b. ( ErrorComponent e
                                  , Stream s
                                  , Token s ~ Char
                                  , Markup1 a
                                  , MarkupTuple a b
                                  , IDoc e s m b) => ParsecT e s m a
parseMarkup1 = do
  let Markup m1 :: Markup a = markup1
  al :: AttrList <- optionalAttrList
  void $ string m1
  void $ char ':'
  b <- parseDoc
  oid :: Maybe SetID <- optional parseDoc
  let oid' = (\(SetID sid) -> sid) <$> oid
  return $ fromMarkupTuple (al, b, oid')
  
optionalAttrList :: (ErrorComponent e, Stream s, Token s ~ Char) => ParsecT e s m AttrList
optionalAttrList = do
  mal <- optional $ do
    al <- parseDoc
    void $ optional newline
    return al
  return $ maybe (AttrList empty) id mal

gobbleSpace :: (ErrorComponent e, Stream s, Token s ~ Char) => ParsecT e s m ()
gobbleSpace = void $ optional $ many spaceChar

newtype IDPathComponent = IDPathComponent (Text) deriving (Eq, Show, IsString)
instance Separated IDPathComponent where 
  sep = "/"
instance (ErrorComponent e, Stream s, Token s ~ Char) => 
  IDoc e s m IDPathComponent where
  parseDoc = do
    ipc <- some $ satisfy (\c ->    c /= '/'
                                 && c /= ']'
                                 && c /= '#'
                                 && c /= '>'
                                 && (not $ isSpace c)
                                 && isPrint c)
    return $ IDPathComponent $ fromString $ ipc

newtype IDPath = IDPath (Vector IDPathComponent) deriving (Eq, Show)
instance (ErrorComponent e, Stream s, Token s ~ Char) => 
  IDoc e s m IDPath where
  parseDoc = do
    void $ char '/'
    comps <- parseSeparated
    return $ IDPath $ comps

newtype IDHash = IDHash (Text) deriving (Eq, Show, IsString)
instance (ErrorComponent e, Stream s, Token s ~ Char) => 
  IDoc e s m IDHash where
  parseDoc = do
    h <- many $ satisfy (\c -> c /= ']' && c /= '>' && isPrint c)
    return $ IDHash $ fromString $ h

instance Twin IDPath IDHash where
  twinSep = "#"

data ID = ID { idPath :: IDPath
             , idHash :: IDHash
             } deriving (Eq, Show)
instance (ErrorComponent e, Stream s, Token s ~ Char) => 
  IDoc e s m ID where
  parseDoc = do 
    (idp, idh) <- parseTwin
    return $ ID { idPath = idp
                , idHash = idh
                }

newtype SetID = SetID IDHash deriving (Eq, Show)
instance Between SetID where
  opener = "[["
  closer = "]]"

instance (ErrorComponent e, Stream s, Token s ~ Char) =>
  IDoc e s m SetID where
  parseDoc = do
    let (Opener op :: Opener SetID) = opener
        (Closer cl :: Closer SetID) = closer
    i <- between (string op) (string cl) parseDoc
    return $ SetID $ i

newtype AttrName = AttrName (Text) deriving (Eq, Show, IsString)
instance Separated AttrName where 
  sep = Sep ","
instance (ErrorComponent e, Stream s, Token s ~ Char) => 
  IDoc e s m AttrName where
  parseDoc = label "attribute name" $ do
    firstChar <- choice [letterChar, char '-']
    rest <- many $ choice [alphaNumChar, char '-']
    return $ AttrName $ fromString $ firstChar : rest

instance (ErrorComponent e, Stream s, Token s ~ Char) =>
  IDoc e s m (Vector AttrName) where
  parseDoc = parseSeparated

instance (ErrorComponent e, Stream s, Token s ~ Char) => 
  Contents e s m (Vector AttrName) where
  newtype Container (Vector AttrName) = AttrList (Vector AttrName) deriving (Eq, Show)
  contentsP = parseDoc
  wrapperP p = AttrList <$> p

type AttrList = Container (Vector AttrName)

instance Between (Vector AttrName) where
  opener = "["
  closer = "]"

instance (ErrorComponent e, Stream s, Token s ~ Char) =>
  IDoc e s m AttrList where
  parseDoc = parseContentsBetween

newtype BlockTitle = BlockTitle (Text) deriving (Eq, Show, IsString)
instance Line BlockTitle where
  lineStarter = "."

instance Textlike BlockTitle where 
  fromText = BlockTitle

instance (ErrorComponent e, Stream s, Token s ~ Char) => 
  IDoc e s m BlockTitle where
  parseDoc = parseLineText

newtype MathText = MathText Text deriving (Eq, Show, IsString)
instance (ErrorComponent e, Stream s, Token s ~ Char) =>
  IDoc e s m MathText where
  parseDoc = do
    mt <- many $ do
      notFollowedBy $ char '$'
      (TM.try $ char '\\' >> char '$') <|> printChar <|> spaceChar
    return $ MathText $ fromString mt

newtype InlineMath = InlineMath MathText deriving (Eq, Show, IsString)
instance (ErrorComponent e, Stream s, Token s ~ Char) =>
  IDoc e s m InlineMath where
  parseDoc = do
    void $ char '$'
    mt <- parseDoc
    void $ char '$'
    return $ InlineMath $ mt

testIM :: IO ()
testIM = parseTest (parseDoc :: ParsecT Dec String Identity InlineMath) "$hel\\$lo$"

newtype CommentLine = CommentLine (Text) deriving (Eq, Show, IsString)
instance Line CommentLine where
  lineStarter = "//"

instance Textlike CommentLine where
  fromText = CommentLine

instance (ErrorComponent e, Stream s, Token s ~ Char) =>
  IDoc e s m CommentLine where
  parseDoc = parseLineText

newtype Protocol = Protocol Text deriving (Eq, Show, IsString)
instance (ErrorComponent e, Stream s, Token s ~ Char) =>
  IDoc e s m Protocol where
  parseDoc = Protocol . fromString <$> (choice $ [string "https", string "http"])

newtype URI = URI Text deriving (Eq, Show, IsString)
instance (ErrorComponent e, Stream s, Token s ~ Char) =>
  IDoc e s m URI where
  parseDoc = URI . fromString <$> (many $ do
                                      notFollowedBy $ char '>' >> char '>'
                                      printChar)

newtype CommonLink = CommonLink (Protocol, URI) deriving (Eq, Show)
instance Twin Protocol URI where
  twinSep = "://"

instance (ErrorComponent e, Stream s, Token s ~ Char) =>
  IDoc e s m CommonLink where
  parseDoc = CommonLink <$> parseTwin

newtype Back = Back (ID) deriving (Eq, Show)
instance (ErrorComponent e, Stream s, Token s ~ Char) =>
  IDoc e s m Back where
  parseDoc = Back <$> parseDoc

newtype Out = Out (CommonLink) deriving (Eq, Show)
instance (ErrorComponent e, Stream s, Token s ~ Char) =>
  IDoc e s m Out where
  parseDoc = Out <$> parseDoc

newtype Internal = Internal (IDHash) deriving (Eq, Show, IsString)
instance (ErrorComponent e, Stream s, Token s ~ Char) =>
  IDoc e s m Internal where
  parseDoc = do
    void $ char '#'
    Internal <$> parseDoc

-- | FIXME: This should handle QText too.
newtype LinkText = LinkText Text deriving (Eq, Show, IsString)
instance Textlike LinkText where
  fromText = LinkText

instance Between LinkText where
  opener = "["
  closer = "]"

instance Escapable LinkText where
  escape = fromList [ Escape ']' ]

instance (ErrorComponent e, Stream s, Token s ~ Char) =>
  IDoc e s m LinkText where
  parseDoc = parseEscapableTextBetween

data LinkT protocol = LinkT { linkText :: Maybe LinkText
                            , linkAttrs :: AttrList
                            , linkLocation :: protocol
                            } deriving (Eq, Show)
instance (ErrorComponent e, Stream s, Token s ~ Char, IDoc e s m protocol) =>
  IDoc e s m (LinkT protocol) where
  parseDoc = label "link between angle-brackets (i.e. <<https://www.independentlearning.science>>)" $ do
    al <- optionalAttrList
    void $ string "<<"
    ll <- parseDoc
    void $ string ">>"
    lt <- optional parseDoc
    return LinkT { linkText = lt
                 , linkAttrs = al
                 , linkLocation = ll
                 }

type BLink = LinkT Back
type OLink = LinkT Out
type ILink = LinkT Internal

data Link = LBLink BLink
          | LOLink OLink
          | LILink ILink deriving (Eq, Show)

testLink :: IO ()
testLink = mapM_ (parseTest (parseDoc :: ParsecT Dec String Identity Link)) [ "<<https://www.independentlearning.science>>"
                                                                            , "<<#hello>>"
                                                                            , "<</Physics/Fun#>>"]

instance (ErrorComponent e, Stream s, Token s ~ Char) =>
  IDoc e s m Link where
  parseDoc = do
    lnk <- eitherP (TM.try parseDoc :: ParsecT e s m ILink) $
           eitherP (TM.try parseDoc :: ParsecT e s m BLink) $
                   (parseDoc :: ParsecT e s m OLink) 
    case lnk of
      Right (Left bl) -> return $ LBLink bl
      Right (Right ol) -> return $ LOLink ol
      Left il -> return $ LILink il

data Bold
data Italic
data Monospace
data Superscript
data Subscript

data QTextT style = QTextT { qtextText :: Text
                           , qtextAttrs :: AttrList
                           } deriving (Eq, Show)

instance ( ErrorComponent e
         , Stream s
         , Token s ~ Char
         , Escapable (QTextT style)
         , Between (QTextT style)
         , Textlike (QTextT style)) =>
  IDoc e s m (QTextT style) where
  parseDoc = do
    al <- optionalAttrList
    qt <- parseEscapableTextBetween :: ParsecT e s m (QTextT style)
    return $ qt { qtextAttrs = al }

instance Textlike (QTextT style) where
  fromText t = QTextT { qtextText = t 
                      , qtextAttrs = AttrList empty
                      }

instance Between (QTextT Bold) where
  opener = "*"
  closer = "*"

instance Escapable (QTextT Bold) where
  escape = fromList [ Escape '*' ]

instance Between (QTextT Italic) where
  opener = "_"
  closer = "_"

instance Escapable (QTextT Italic) where
  escape = fromList [ Escape '_' ]

instance Between (QTextT Monospace) where
  opener = "`"
  closer = "`"

instance Escapable (QTextT Monospace) where
  escape = fromList [ Escape '`' ]

instance Between (QTextT Superscript) where
  opener = "^"
  closer = "^"

instance Escapable (QTextT Superscript) where
  escape = fromList [ Escape '^' ]

instance Between (QTextT Subscript) where
  opener = "~"
  closer = "~"

instance Escapable (QTextT Subscript) where
  escape = fromList [ Escape '~' ]

type BoldText = QTextT Bold
type ItalicText = QTextT Italic
type MonospaceText = QTextT Monospace
type SuperscriptText = QTextT Superscript
type SubscriptText = QTextT Subscript

data QText = QTBoldText BoldText
           | QTItalicText ItalicText
           | QTMonospaceText MonospaceText
           | QTSuperscriptText SuperscriptText
           | QTSubscriptText SubscriptText deriving (Eq, Show)

instance (ErrorComponent e, Stream s, Token s ~ Char) =>
  IDoc e s m QText where
  parseDoc = do
    qt <- eitherP (TM.try parseDoc :: ParsecT e s m BoldText) $ 
          eitherP (TM.try parseDoc :: ParsecT e s m ItalicText) $ 
          eitherP (TM.try parseDoc :: ParsecT e s m MonospaceText) $
          eitherP (TM.try parseDoc :: ParsecT e s m SuperscriptText) $
                  (parseDoc :: ParsecT e s m SubscriptText)
    case qt of
      Left bt -> return $ QTBoldText bt
      Right nbt -> 
        case nbt of 
          Left it -> return $ QTItalicText it
          Right nit ->
            case nit of 
              Left mt -> return $ QTMonospaceText mt
              Right nmt ->
                case nmt of
                  Left supt -> return $ QTSuperscriptText supt
                  Right subt -> return $ QTSubscriptText subt

data Footnote = Footnote { footnoteContent :: Paragraph
                         , footnoteAttrs   :: AttrList
                         , footnoteID      :: Maybe SetID
                         } deriving (Eq, Show)
instance Markup1 Footnote where
  markup1 = "footnote"

instance MarkupTuple Footnote Paragraph where
  fromMarkupTuple (al, cont, oid) = Footnote { footnoteContent = cont
                                             , footnoteAttrs = al
                                             , footnoteID = SetID <$> oid }

instance (ErrorComponent e, Stream s, Token s ~ Char) =>
  IDoc e s m Footnote where
  parseDoc = parseMarkup1

data FootnoteRef = FootnoteRef { footnoteRefID :: ILink
                               , footnoteRefAttrs :: AttrList
                               , footnoteRefIDID :: Maybe SetID
                               } deriving (Eq, Show)
instance Markup1 FootnoteRef where
  markup1 = "footnoteref"

instance MarkupTuple FootnoteRef ILink where
  fromMarkupTuple (al, cont, oid) = FootnoteRef { footnoteRefID = cont
                                                , footnoteRefAttrs = al
                                                , footnoteRefIDID = SetID <$> oid }

instance (ErrorComponent e, Stream s, Token s ~ Char) =>
  IDoc e s m FootnoteRef where
  parseDoc = parseMarkup1

newtype TextWord = TextWord Text deriving (Eq, Show, IsString)
instance Escapable TextWord where
  escape = fromList [ Escape '$'
                    , Escape '^'
                    , Escape '~'
                    , Escape '*'
                    , Escape '_'
                    , Escape '['
                    , Escape ']'
                    , Escape '<'
                    , Escape '>'
                    ]

newtype PlainText = PlainText Text deriving (Eq, Show, IsString)
instance (ErrorComponent e, Stream s, Token s ~ Char) =>
  IDoc e s m PlainText where
  parseDoc = label "some plain text" $ do
    pt <- some $ do
      notFollowedBy (paraDelim <|> (void (parseDoc :: ParsecT e s m SimpleContent)))
      printChar <|> spaceChar
    return $ PlainText $ fromString pt

testPT :: IO ()
testPT = parseTest ((parseDoc :: ParsecT Dec String Identity PlainText) >> (parseDoc :: ParsecT Dec String Identity BoldText)) "hello neighbor *there*"

data SimpleContent = SCInlineMath InlineMath
                   | SCLink Link
                   | SCFootnote Footnote
                   | SCFootnoteRef FootnoteRef
                   | SCQText QText deriving (Eq, Show)

instance (ErrorComponent e, Stream s, Token s ~ Char) => 
  IDoc e s m SimpleContent where
  parseDoc = label "one of: some InlineMath, a Link, a Footnote, a FootnoteRef, some QText, some PlainText" $ do
    sc <- eitherP (TM.try parseDoc :: ParsecT e s m InlineMath) $ 
          eitherP (TM.try parseDoc :: ParsecT e s m Link) $ 
          eitherP (TM.try parseDoc :: ParsecT e s m Footnote) $
          eitherP (TM.try parseDoc :: ParsecT e s m FootnoteRef) $
                  (parseDoc :: ParsecT e s m QText)
    case sc of
      Left im -> return $ SCInlineMath im
      Right nim ->
        case nim of
          Left l -> return $ SCLink l
          Right nl ->
            case nl of
              Left fn -> return $ SCFootnote fn
              Right nfn ->
                case nfn of 
                  Left fnr -> return $ SCFootnoteRef fnr
                  Right qt -> return $ SCQText qt

data ParagraphContent = PCSimpleContent SimpleContent 
                      | PCPlainText PlainText deriving (Eq, Show)

instance (ErrorComponent e, Stream s, Token s ~ Char) => 
  IDoc e s m ParagraphContent where
  parseDoc = do
    pc <-eitherP (TM.try parseDoc :: ParsecT e s m SimpleContent) $
                 (parseDoc :: ParsecT e s m PlainText)
    case pc of
      Left sc -> return $ PCSimpleContent sc
      Right pt -> return $ PCPlainText pt

newtype Paragraph = Paragraph (Vector ParagraphContent, Maybe SetID) deriving (Eq, Show)
instance (ErrorComponent e, Stream s, Token s ~ Char) => 
  IDoc e s m Paragraph where
  parseDoc = do
    pcs <- some parseDoc
    gobbleSpace
    oid <- optional parseDoc
    return $ Paragraph (fromList pcs, oid)

testParagraph :: IO ()
testParagraph = mapM_ 
  (parseTest (parseDoc :: ParsecT Dec String Identity Paragraph)) [ "*hello neighbor*"
                                                                  , "hello *neighbor*\n\nhello again!"
                                                                  , "hello <<#ilink>>[neighbor]"
                                                                  , "Inline math is done just using\nnormal latex by doing $f(x) = \\exp\n(-x^2)$.  Display mode is done by using a _math block_, like so:\n\n"
                                                                  ]

data Unordered = Unordered deriving (Eq, Show)
instance (ErrorComponent e, Stream s, Token s ~ Char) => 
  IDoc e s m Unordered where
  parseDoc = do
    void $ string "- "
    return $ Unordered

data Ordered = Ordered deriving (Eq, Show)
instance (ErrorComponent e, Stream s, Token s ~ Char) => 
  IDoc e s m Ordered where
  parseDoc = do
    void $ string ". "
    return $ Ordered

newtype Labelled = Labelled (Text) deriving (Eq, Show, IsString)
instance (ErrorComponent e, Stream s, Token s ~ Char) => 
  IDoc e s m Labelled where
  parseDoc = do
    void $ string ": "
    l <- some $ do
      notFollowedBy $ string ":: "
      printChar
    void $ string ":: "
    return $ Labelled $ fromString l

data ListItem labelType = ListItem { listItemContents :: Paragraph
                                   , listItemAttrs    :: AttrList
                                   , listItemID       :: Maybe SetID
                                   , listItemLabel    :: labelType
                                   } deriving (Eq, Show)
instance (ErrorComponent e, Stream s, Token s ~ Char, IDoc e s m labelType) => 
  IDoc e s m (ListItem labelType) where
  parseDoc = do
    al <- optionalAttrList
    il <- parseDoc
    ic <- some parseDoc
    oid <- optional parseDoc
    return $ ListItem { listItemContents = Paragraph (fromList ic, Nothing)
                      , listItemAttrs = al
                      , listItemID = oid
                      , listItemLabel = il
                      }

listDelim :: (ErrorComponent e, Stream s, Token s ~ Char) => ParsecT e s m ()
listDelim = void $ string "\n+\n"

data ListT labelType = ListT { listItems :: Vector (ListItem labelType)
                             , listAttrs :: AttrList
                             , listID    :: Maybe SetID
                             } deriving (Eq, Show)
instance (ErrorComponent e, Stream s, Token s ~ Char, IDoc e s m labelType) => 
  IDoc e s m (ListT labelType) where
  parseDoc = do
    al <- do
      oal <- optionalAttrList
      void $ optional newline
      return oal
    li <- sepBy1 parseDoc listDelim
    oid <- optional parseDoc
    return $ ListT { listItems = fromList li
                   , listAttrs = al
                   , listID = oid
                   }

type UList = ListT Unordered
type OList = ListT Ordered
type LList = ListT Labelled
  
data List = LUList UList
          | LOList OList
          | LLList LList deriving (Eq, Show)
instance (ErrorComponent e, Stream s, Token s ~ Char) => 
  IDoc e s m List where
  parseDoc = do
    l <- eitherP (TM.try parseDoc :: ParsecT e s m UList) $
         eitherP (TM.try parseDoc :: ParsecT e s m OList) $
                 (parseDoc :: ParsecT e s m LList)
    case l of
      Left ul -> return $ LUList ul
      Right nul -> case nul of
        Left ol -> return $ LOList ol
        Right ll -> return $ LLList ll

randomText :: (ErrorComponent e, Stream s, Token s ~ Char) => ParsecT e s m Text
randomText = fmap (fromString . initEx) $ some $ do 
  notFollowedBy blockEnd
  printChar <|> spaceChar

newtype ImageLink = ImageLink Text deriving (Eq, Show, IsString)
newtype VideoLink = VideoLink Text deriving (Eq, Show, IsString)
newtype YouTubeLink = YouTubeLink Text deriving (Eq, Show, IsString)
newtype BibliographyContent = BibliographyContent Text deriving (Eq, Show, IsString)
instance (ErrorComponent e, Stream s, Token s ~ Char) => 
  IDoc e s m BibliographyContent where
  parseDoc = do
    traceShowM ("WARNING: calling parseDoc :: BibliographyContent, which is not really totally implemented." :: String)
    t <- randomText
    return $ BibliographyContent $ t

newtype PrerexItem = PrerexItem IDPath deriving (Eq, Show)
instance (ErrorComponent e, Stream s, Token s ~ Char) => 
  IDoc e s m PrerexItem where
  parseDoc = PrerexItem <$> parseDoc

newtype BlockType a = BlockType Text deriving (Eq, Show, IsString)
instance Line (BlockType a) where lineStarter = "@"
instance Textlike (BlockType a) where fromText = BlockType

instance (ErrorComponent e, Stream s, Token s ~ Char, BlockName a) => 
  IDoc e s m (BlockType a) where
  parseDoc = do
    let (LineStarter ls) :: LineStarter (BlockType a) = lineStarter
        (BlockType   n)  :: BlockType   a = name
    void $ string ls
    void $ space
    bt <- string $ unpack n
    void $ newline
    return $ BlockType $ fromString bt

class BlockName a where
  name :: BlockType a

newtype Prerex = Prerex (Vector PrerexItem) deriving (Eq, Show)
instance BlockName Prerex where name = "prerex"
instance (ErrorComponent e, Stream s, Token s ~ Char) => 
  IDoc e s m Prerex where
  parseDoc = do
    pis <- some $ do
      notFollowedBy blockEnd
      pii <- parseDoc
      void $ newline
      return pii
    return $ Prerex $ fromList pis

blockBeg :: (ErrorComponent e, Stream s, Token s ~ Char) =>  ParsecT e s m ()
blockBeg = void $ string "--\n"

blockEnd :: (ErrorComponent e, Stream s, Token s ~ Char) =>  ParsecT e s m ()
blockEnd = void $ string "--\n"

newtype Math = Math Text deriving (Eq, Show, IsString)
instance BlockName Math where name = "math"
instance (ErrorComponent e, Stream s, Token s ~ Char) => 
  IDoc e s m Math where
  parseDoc = Math <$> randomText

type Equation = Text

newtype EqnArray = EqnArray (Vector Equation) deriving (Eq, Show)
instance BlockName EqnArray where name = "eqnarray"
instance (ErrorComponent e, Stream s, Token s ~ Char) =>
  IDoc e s m EqnArray where
  parseDoc = do
    es <- some $ do
      notFollowedBy blockEnd
      e <- some printChar
      void $ newline
      return $ fromString e
    return $ EqnArray $ fromList es

someCC :: (ErrorComponent e, Stream s, Token s ~ Char) => ParsecT e s m (Vector ComplexContent)
someCC = fmap fromList $ some $ do
  notFollowedBy blockEnd
  parseDoc

newtype Theorem = Theorem (Vector ComplexContent) deriving (Eq, Show)
instance BlockName Theorem where name = "theorem"
instance (ErrorComponent e, Stream s, Token s ~ Char) =>
  IDoc e s m Theorem where
  parseDoc = Theorem <$> someCC

newtype Proof = Proof (Vector ComplexContent) deriving (Eq, Show)
instance BlockName Proof where name = "proof"
instance (ErrorComponent e, Stream s, Token s ~ Char) =>
  IDoc e s m Proof where
  parseDoc = Proof <$> someCC

newtype Quote = Quote Text deriving (Eq, Show, IsString)
instance BlockName Quote where name = "quote"
instance (ErrorComponent e, Stream s, Token s ~ Char) =>
  IDoc e s m Quote where
  parseDoc = Quote <$> randomText

newtype Code = Code Text deriving (Eq, Show, IsString)
instance BlockName Code where name = "code"
instance (ErrorComponent e, Stream s, Token s ~ Char) =>
  IDoc e s m Code where
  parseDoc = Code <$> randomText

newtype Image = Image OLink deriving (Eq, Show)
instance BlockName Image where name = "image"
instance (ErrorComponent e, Stream s, Token s ~ Char) =>
  IDoc e s m Image where
  parseDoc = Image <$> parseDoc

newtype Video = Video OLink deriving (Eq, Show)
instance BlockName Video where name = "video"
instance (ErrorComponent e, Stream s, Token s ~ Char) =>
  IDoc e s m Video where
  parseDoc = Video <$> parseDoc

newtype YouTube = YouTube OLink deriving (Eq, Show)
instance BlockName YouTube where name = "youtube"
instance (ErrorComponent e, Stream s, Token s ~ Char) =>
  IDoc e s m YouTube where
  parseDoc = YouTube <$> parseDoc

newtype Aside = Aside (Maybe PrerexBlock, Vector ComplexContent) deriving (Eq, Show)
instance BlockName Aside where name = "aside"
instance (ErrorComponent e, Stream s, Token s ~ Char) =>
  IDoc e s m Aside where
  parseDoc = do
    mpb <- optional parseDoc
    ccs <- someCC
    return $ Aside (mpb, ccs)

newtype Admonition = Admonition (Vector ComplexContent) deriving (Eq, Show)
instance BlockName Admonition where name = "admonition"
instance (ErrorComponent e, Stream s, Token s ~ Char) =>
  IDoc e s m Admonition where
  parseDoc = Admonition <$> someCC

newtype Sidebar = Sidebar (Vector ComplexContent) deriving (Eq, Show)
instance BlockName Sidebar where name = "sidebar"
instance (ErrorComponent e, Stream s, Token s ~ Char) =>
  IDoc e s m Sidebar where
  parseDoc = Sidebar <$> someCC

newtype Example = Example (Vector ComplexContent) deriving (Eq, Show)
instance BlockName Example where name = "example"
instance (ErrorComponent e, Stream s, Token s ~ Char) =>
  IDoc e s m Example where
  parseDoc = Example <$> someCC

newtype Exercise = Exercise (Vector ComplexContent) deriving (Eq, Show)
instance BlockName Exercise where name = "exercise"
instance (ErrorComponent e, Stream s, Token s ~ Char) =>
  IDoc e s m Exercise where
  parseDoc = Exercise <$> someCC

newtype Bibliography = Bibliography (Vector BibliographyContent) deriving (Eq, Show)
instance BlockName Bibliography where name = "bibliography"
instance (ErrorComponent e, Stream s, Token s ~ Char) => 
  IDoc e s m Bibliography where
  parseDoc = do
    bibs <- some $ do
      notFollowedBy blockEnd
      parseDoc
    return $ Bibliography $ fromList bibs

data BlockT blockType = BlockT { blockContents :: blockType
                               , blockType  :: BlockType blockType
                               , blockAttrs :: AttrList
                               , blockTitle :: Maybe BlockTitle
                               , blockID    :: Maybe SetID
                               } deriving (Eq, Show)
instance (ErrorComponent e, Stream s, Token s ~ Char, IDoc e s m bType, BlockName bType) =>
  IDoc e s m (BlockT bType) where
  parseDoc = do
    al <- optionalAttrList
    bty <- parseDoc
    bt <- optional parseDoc
    bc <- between blockBeg blockEnd parseDoc
    bi <- optional parseDoc
    return $ BlockT { blockContents = bc
                    , blockType = bty
                    , blockAttrs = al
                    , blockTitle = bt
                    , blockID = bi
                    }

type PrerexBlock = BlockT Prerex
type MathBlock = BlockT Math
type EqnArrayBlock = BlockT EqnArray
type TheoremBlock = BlockT Theorem
type ProofBlock = BlockT Proof
type QuoteBlock = BlockT Quote
type CodeBlock = BlockT Code
type ImageBlock = BlockT Image
type VideoBlock = BlockT Video
type YouTubeBlock = BlockT YouTube
type AsideBlock = BlockT Aside
type AdmonitionBlock = BlockT Admonition
type SidebarBlock = BlockT Sidebar
type ExampleBlock = BlockT Example
type ExerciseBlock = BlockT Exercise
type BibliographyBlock = BlockT Bibliography

data Block = BMathBlock MathBlock
           | BEqnArrayBlock EqnArrayBlock
           | BTheoremBlock TheoremBlock
           | BProofBlock ProofBlock
           | BPrerexBlock PrerexBlock
           | BQuoteBlock QuoteBlock
           | BCodeBlock CodeBlock
           | BImageBlock ImageBlock
           | BVideoBlock VideoBlock
           | BAdmonitionBlock AdmonitionBlock
           | BAsideBlock AsideBlock
           | BYouTubeBlock YouTubeBlock
           | BSidebarBlock SidebarBlock
           | BExampleBlock ExampleBlock
           | BExerciseBlock ExerciseBlock
           | BBibliographyBlock BibliographyBlock deriving (Eq, Show)

instance (ErrorComponent e, Stream s, Token s ~ Char) => 
  IDoc e s m Block where
  parseDoc = do
    b <- eitherP (TM.try parseDoc :: ParsecT e s m MathBlock) $
         eitherP (TM.try parseDoc :: ParsecT e s m EqnArrayBlock) $
         eitherP (TM.try parseDoc :: ParsecT e s m TheoremBlock) $
         eitherP (TM.try parseDoc :: ParsecT e s m ProofBlock) $
         eitherP (TM.try parseDoc :: ParsecT e s m PrerexBlock) $
         eitherP (TM.try parseDoc :: ParsecT e s m QuoteBlock) $
         eitherP (TM.try parseDoc :: ParsecT e s m CodeBlock) $
         eitherP (TM.try parseDoc :: ParsecT e s m ImageBlock) $
         eitherP (TM.try parseDoc :: ParsecT e s m VideoBlock) $
         eitherP (TM.try parseDoc :: ParsecT e s m AdmonitionBlock) $
         eitherP (TM.try parseDoc :: ParsecT e s m AsideBlock) $
         eitherP (TM.try parseDoc :: ParsecT e s m YouTubeBlock) $
         eitherP (TM.try parseDoc :: ParsecT e s m SidebarBlock) $
         eitherP (TM.try parseDoc :: ParsecT e s m ExampleBlock) $
         eitherP (TM.try parseDoc :: ParsecT e s m ExerciseBlock) $
                 (parseDoc :: ParsecT e s m BibliographyBlock)
    case b of
      Left mb -> return $ BMathBlock mb
      Right nmb -> case nmb of
        Left eab -> return $ BEqnArrayBlock eab
        Right neab -> case neab of
          Left tb -> return $ BTheoremBlock tb
          Right ntb -> case ntb of
            Left pb -> return $ BProofBlock pb
            Right npb -> case npb of
              Left prb -> return $ BPrerexBlock prb
              Right nprb -> case nprb of
                Left qb -> return $ BQuoteBlock qb
                Right nqb -> case nqb of
                  Left cb -> return $ BCodeBlock cb
                  Right ncb -> case ncb of
                    Left ib -> return $ BImageBlock ib
                    Right nib -> case nib of
                      Left vb -> return $ BVideoBlock vb
                      Right nvb -> case nvb of
                        Left ab -> return $ BAdmonitionBlock ab
                        Right nab -> case nab of
                          Left asb -> return $ BAsideBlock asb
                          Right nasb -> case nasb of
                            Left ytb -> return $ BYouTubeBlock ytb
                            Right nytb -> case nytb of
                              Left sb -> return $ BSidebarBlock sb
                              Right nsb -> case nsb of
                                Left eb -> return $ BExampleBlock eb
                                Right neb -> case neb of
                                  Left exeb -> return $ BExerciseBlock exeb
                                  Right bb -> return $ BBibliographyBlock bb

newtype Title = Title Text deriving (Eq, Show, IsString)
instance Line Title where lineStarter = "="
instance Textlike Title where fromText = Title

newtype Section = Section Text deriving (Eq, Show, IsString)
instance Line Section where lineStarter = "=="
instance Textlike Section where fromText = Section

newtype Subsection = Subsection Text deriving (Eq, Show, IsString)
instance Line Subsection where lineStarter = "==="
instance Textlike Subsection where fromText = Subsection

data Heading htype = Heading { hContents :: htype
                             , hID       :: Maybe SetID
                             , hAttrs    :: AttrList
                             } deriving (Eq, Show)

type SectionHeading = Heading Section
type SubsectionHeading = Heading Subsection

instance (ErrorComponent e, Stream s, Token s ~ Char, Line htype, Textlike htype) => 
  IDoc e s m (Heading htype) where
  parseDoc = do
    al <- optionalAttrList
    t <- parseLineText
    oid <- optional parseDoc
    return $ Heading { hContents = t
                     , hID = oid
                     , hAttrs = al
                     }

data ComplexContent = CCBlock Block
                    | CCList List
                    | CCParagraph Paragraph deriving (Eq, Show)
instance (ErrorComponent e, Stream s, Token s ~ Char) => 
  IDoc e s m ComplexContent where
  parseDoc = do
    cc <- eitherP (TM.try parseDoc :: ParsecT e s m Block) $
          eitherP (TM.try parseDoc :: ParsecT e s m List) $
                  (parseDoc :: ParsecT e s m Paragraph)
    case cc of
      Left b -> return $ CCBlock b
      Right nb ->
        case nb of
          Left l -> return $ CCList l
          Right p -> return $ CCParagraph p

data TopLevelContent = TLCSubsectionHeading SubsectionHeading 
                     | TLCSectionHeading SectionHeading
                     | TLCCommentLine CommentLine
                     | TLCComplexContent ComplexContent deriving (Eq, Show)
instance (ErrorComponent e, Stream s, Token s ~ Char) => 
  IDoc e s m TopLevelContent where
  parseDoc = do
    tlc <- eitherP (TM.try parseDoc :: ParsecT e s m SubsectionHeading) $
           eitherP (TM.try parseDoc :: ParsecT e s m SectionHeading) $
           eitherP (TM.try parseDoc :: ParsecT e s m CommentLine) $
                   (parseDoc :: ParsecT e s m ComplexContent)
    case tlc of
      Left ssh -> return $ TLCSubsectionHeading ssh
      Right nssh -> case nssh of 
        Left sh -> return $ TLCSectionHeading sh
        Right nsh -> case nsh of
          Left cl -> return $ TLCCommentLine cl
          Right cc -> return $ TLCComplexContent cc

data Doc = Doc { docTitle :: Heading Title
               , docPrerex :: PrerexBlock
               , docContents :: Vector TopLevelContent
               } deriving (Eq, Show)
instance (ErrorComponent e, Stream s, Token s ~ Char) => 
  IDoc e s m Doc where
  parseDoc = do
    dt <- parseDoc
    gobbleSpace
    dp <- parseDoc
    gobbleSpace
    dcs <- many $ do
      dc <- parseDoc
      traceShowM dc
      gobbleSpace
      return dc
    gobbleSpace
    eof
    return $ Doc { docTitle = dt
                 , docPrerex = dp
                 , docContents = fromList dcs
                 }
