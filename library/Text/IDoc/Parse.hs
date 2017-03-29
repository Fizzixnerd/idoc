{-# LANGUAGE StandaloneDeriving #-}
-- | Parse.hs
-- Author: Matt Walker
-- License: https://opensource.org/licenses/BSD-2-Clause
-- Created: Mar 23, 2017
-- Summary: 

{-# LANGUAGE Arrows #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE DataKinds #-}

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
  ret <- sepBy parseDoc $ string s
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
  mal <- optional parseDoc
  return $ maybe (AttrList empty) id mal

newtype IDPathComponent = IDPathComponent (Text) deriving (Eq, Show, IsString)
instance Separated IDPathComponent where 
  sep = "/"
instance (ErrorComponent e, Stream s, Token s ~ Char) => 
  IDoc e s m IDPathComponent where
  parseDoc = do
    ipc <- many $ satisfy (\c ->    c /= '/'
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

testIM = parseTest (parseDoc :: ParsecT Dec String Identity InlineMath) "$hel\\$lo$"

-- instance (ErrorComponent e, Stream s, Token s ~ Char) => 
--   IDoc e s m InlineMath where
--   parseDoc = 

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

testLink = mapM_ (parseTest (parseDoc :: ParsecT Dec String Identity Link)) [ "<<https://www.independentlearning.science>>"
                                                                            , "<<#hello>>"
                                                                            , "<</Physics/Fun#>>"]

instance (ErrorComponent e, Stream s, Token s ~ Char) =>
  IDoc e s m Link where
  parseDoc = do
    lnk <- eitherP (traceShowM "ilink" >> TM.try parseDoc :: ParsecT e s m ILink) $
           eitherP (traceShowM "blink" >> TM.try parseDoc :: ParsecT e s m BLink) $
                   (traceShowM "olink" >> parseDoc :: ParsecT e s m OLink) 
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

instance Between (QTextT Bold) where
  opener = "*"
  closer = "*"

instance Escapable (QTextT Bold) where
  escape = fromList [ Escape '*' ]

instance Textlike (QTextT a) where
  fromText t = QTextT { qtextText = t 
                      , qtextAttrs = AttrList empty
                      }

instance (ErrorComponent e, Stream s, Token s ~ Char) =>
  IDoc e s m (QTextT Bold) where
  parseDoc = traceShowM "bText" >> do
    al <- optionalAttrList
    qt <- parseEscapableTextBetween :: ParsecT e s m (QTextT Bold)
    return $ qt { qtextAttrs = al }

instance Between (QTextT Italic) where
  opener = "_"
  closer = "_"

instance Escapable (QTextT Italic) where
  escape = fromList [ Escape '_' ]

instance (ErrorComponent e, Stream s, Token s ~ Char) =>
  IDoc e s m (QTextT Italic) where
  parseDoc = do
    al <- optionalAttrList
    qt <- parseEscapableTextBetween :: ParsecT e s m (QTextT Italic)
    return $ qt { qtextAttrs = al }

instance Between (QTextT Monospace) where
  opener = "`"
  closer = "`"

instance Escapable (QTextT Monospace) where
  escape = fromList [ Escape '`' ]

instance (ErrorComponent e, Stream s, Token s ~ Char) =>
  IDoc e s m (QTextT Monospace) where
  parseDoc = do
    al <- optionalAttrList
    qt <- parseEscapableTextBetween :: ParsecT e s m (QTextT Monospace)
    return $ qt { qtextAttrs = al }

instance Between (QTextT Superscript) where
  opener = "^"
  closer = "^"

instance Escapable (QTextT Superscript) where
  escape = fromList [ Escape '^' ]

instance (ErrorComponent e, Stream s, Token s ~ Char) =>
  IDoc e s m (QTextT Superscript) where
  parseDoc = do
    al <- optionalAttrList
    qt <- parseEscapableTextBetween :: ParsecT e s m (QTextT Superscript)
    return $ qt { qtextAttrs = al }

instance Between (QTextT Subscript) where
  opener = "~"
  closer = "~"

instance Escapable (QTextT Subscript) where
  escape = fromList [ Escape '~' ]

instance (ErrorComponent e, Stream s, Token s ~ Char) =>
  IDoc e s m (QTextT Subscript) where
  parseDoc = do
    al <- optionalAttrList
    qt <- parseEscapableTextBetween :: ParsecT e s m (QTextT Subscript)
    return $ qt { qtextAttrs = al }

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
    qt <- eitherP (parseDoc :: ParsecT e s m BoldText) $ 
          eitherP (parseDoc :: ParsecT e s m ItalicText) $ 
          eitherP (parseDoc :: ParsecT e s m MonospaceText) $
          eitherP (parseDoc :: ParsecT e s m SuperscriptText) $
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
                         , footnoteID      :: Maybe IDHash
                         } deriving (Eq, Show)
instance Markup1 Footnote where
  markup1 = "footnote"

instance MarkupTuple Footnote Paragraph where
  fromMarkupTuple (al, cont, oid) = Footnote { footnoteContent = cont
                                             , footnoteAttrs = al
                                             , footnoteID = oid }

instance (ErrorComponent e, Stream s, Token s ~ Char) =>
  IDoc e s m Footnote where
  parseDoc = parseMarkup1

data FootnoteRef = FootnoteRef { footnoteRefID :: ILink
                               , footnoteRefAttrs :: AttrList
                               , footnoteRefIDID :: Maybe IDHash
                               } deriving (Eq, Show)
instance Markup1 FootnoteRef where
  markup1 = "footnoteref"

instance MarkupTuple FootnoteRef ILink where
  fromMarkupTuple (al, cont, oid) = FootnoteRef { footnoteRefID = cont
                                                , footnoteRefAttrs = al
                                                , footnoteRefIDID = oid }

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

instance (ErrorComponent e, Stream s, Token s ~ Char) => 
  IDoc e s m TextWord where
  parseDoc = traceShowM "tword" >> do
    let es = (\(Escape e :: Escape TextWord) -> e) <$> escape
    cs <- someTill (do
      traceShowM "choice" 
      choice [ foldl' (<|>) empty $
               fromList [ char '\\' >> char '\\' ]
               <> (fmap (\e -> char '\\' >> char e) es)
             , do
                 traceShowM "satisfy" 
                 satisfy (\c -> not (elem c es) && isPrint c)
             ])
          (traceShowM "spacechar" >> spaceChar)
    return $ TextWord $ fromString cs

testTW = parseTest (parseDoc :: ParsecT Dec String Identity TextWord) "*hello* neighbor"

parseTextWordsNoNewPara :: (ErrorComponent e, Stream s, Token s ~ Char) => ParsecT e s m (Vector TextWord)
parseTextWordsNoNewPara = do
  tws <- some parseDoc
  return $ fromList tws
  
newtype PlainText = PlainText Text deriving (Eq, Show, IsString)
instance (ErrorComponent e, Stream s, Token s ~ Char) =>
  IDoc e s m PlainText where
  parseDoc = traceShowM "ptext" >> label "some plain text" $ 
             PlainText 
             <$> unwords 
             <$> (fmap (\(TextWord tw) -> tw)) 
             <$> parseTextWordsNoNewPara

testPT = parseTest ((parseDoc :: ParsecT Dec String Identity PlainText) >> (parseDoc :: ParsecT Dec String Identity BoldText)) "hello neighbor *there*"

data SimpleContent = SCInlineMath InlineMath
                   | SCLink Link
                   | SCFootnote Footnote
                   | SCFootnoteRef FootnoteRef
                   | SCQText QText 
                   | SCPlainText PlainText deriving (Eq, Show)

instance (ErrorComponent e, Stream s, Token s ~ Char) => 
  IDoc e s m SimpleContent where
  parseDoc = label "one of: some InlineMath, a Link, a Footnote, a FootnoteRef, some QText, some PlainText" $ do
    sc <- eitherP (parseDoc :: ParsecT e s m InlineMath) $ 
          eitherP (parseDoc :: ParsecT e s m Link) $ 
          eitherP (parseDoc :: ParsecT e s m Footnote) $
          eitherP (parseDoc :: ParsecT e s m FootnoteRef) $
          eitherP (parseDoc :: ParsecT e s m QText) $
                  (parseDoc :: ParsecT e s m PlainText)
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
                  Right nfnr ->
                    case nfnr of
                      Left qt -> return $ SCQText qt
                      Right pt -> return $ SCPlainText pt

testSC :: IO ()
testSC = mapM_ (parseTest (parseDoc :: ParsecT Dec String Identity SimpleContent)) [ "hello neighbor\n\n" ]

-- | Unlike everything else, `parseDoc' for Paragraphs doesn't gobble
-- the whitespace after it.  This will change.
newtype Paragraph = Paragraph (Vector SimpleContent) deriving (Eq, Show)
instance (ErrorComponent e, Stream s, Token s ~ Char) => 
  IDoc e s m Paragraph where
  parseDoc = do
    -- traceShowM "ParseDoc :: Paragraph"
    scs <- many parseDoc
    return $ Paragraph $ fromList $ scs

testParagraph :: IO ()
testParagraph = mapM_ 
  (parseTest (parseDoc :: ParsecT Dec String Identity Paragraph)) [ "*hello neighbor*"
                                                                  , "hello *neighbor*\n\nhello again!"
                                                                  , "hello <<#ilink>>[neighbor]"
                                                                  , "Inline math is done just using normal latex by doing $f(x) = \\exp\n(-x^2)$.  Display mode is done by using a _math block_, like so:\n\n"
                                                                  ]


data Unordered
deriving instance Eq Unordered
deriving instance Show Unordered

data Ordered 
deriving instance Eq Ordered
deriving instance Show Ordered

newtype Labelled = Labelled (Text) deriving (Eq, Show, IsString)

data ListItem labelType = ListItem { listItemContents :: Vector ComplexContent
                                   , listItemAttrs    :: AttrList
                                   , listItemID       :: ID
                                   , listItemLabel    :: labelType
                                   } deriving (Eq, Show)

data ListT labelType = ListT { listItems :: Vector (ListItem labelType)
                             , listAttrs :: AttrList
                             } deriving (Eq, Show)

type UList = ListT Unordered
type OList = ListT Ordered
type LList = ListT Labelled
  
data List = LUList UList
          | LOList OList
          | LLList LList deriving (Eq, Show)

newtype ImageLink = ImageLink Text deriving (Eq, Show, IsString)
newtype VideoLink = VideoLink Text deriving (Eq, Show, IsString)
newtype YouTubeLink = YouTubeLink Text deriving (Eq, Show, IsString)
newtype BibliographyLine = BibliographyLine Text deriving (Eq, Show, IsString)

newtype PrerexItem = PrerexItem IDPath deriving (Eq, Show)

newtype Prerex = Prerex (Vector PrerexItem) deriving (Eq, Show)
newtype Math = Math (Text) deriving (Eq, Show, IsString)
newtype Quote = Quote (Text) deriving (Eq, Show, IsString)
newtype Code = Code (Text) deriving (Eq, Show, IsString)
newtype Image = Image (ImageLink) deriving (Eq, Show)
newtype Video = Video (VideoLink) deriving (Eq, Show)
newtype YouTube = YouTube (YouTubeLink) deriving (Eq, Show)
newtype Aside = Aside (PrerexBlock, Vector ComplexContent) deriving (Eq, Show)
newtype Admonition = Admonition (Vector ComplexContent) deriving (Eq, Show)
newtype Sidebar = Sidebar (Vector ComplexContent) deriving (Eq, Show)
newtype Example = Example (Vector ComplexContent) deriving (Eq, Show)
newtype Exercise = Exercise (Vector ComplexContent) deriving (Eq, Show)
newtype Bibliography = Bibliography (Vector BibliographyLine) deriving (Eq, Show)

data BlockT blockType = BlockT { blockContents :: blockType
                               , blockAttrs :: AttrList
                               , blockTitle :: Maybe BlockTitle
                               , blockID    :: Maybe ID
                               } deriving (Eq, Show)

type PrerexBlock = BlockT Prerex
type MathBlock = BlockT Math
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

data Heading = Heading { hHeading :: Text
                       , hLevel   :: Int
                       , hId      :: Maybe ID
                       , hAttrs   :: Maybe AttrList
                       } deriving (Eq, Show)

data ComplexContent = CCParagraph Paragraph
                    | CCBlock Block
                    | CCList List deriving (Eq, Show)

