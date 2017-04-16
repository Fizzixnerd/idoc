-- | Parse.hs
-- Author: Matt Walker
-- License: https://opensource.org/licenses/BSD-2-Clause
-- Created: Mar 23, 2017
-- Summary: 
{-# OPTIONS_GHC -fno-warn-orphans #-}

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
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DeriveGeneric #-}

module Text.IDoc.Parse where

-- | FIXME: if a paragraph ends with a QText, it doesn't get rendered properly.
import ClassyPrelude
import Text.Megaparsec as TM
import Data.Char
import qualified Text.PrettyPrint.GenericPretty as GP
import qualified Data.List.NonEmpty as NE

instance GP.Out a => GP.Out (Vector a) where
  doc = GP.doc . toList
  docPrec n  = GP.docPrec n . toList 

instance GP.Out Text where
  doc = GP.doc . unpack
  docPrec n = GP.docPrec n . unpack

class ILS e s m a where
  ils :: ParsecT e s m a

start :: (ErrorComponent e, Stream s, Token s ~ Char) => 
  ParsecT e s m b -> ParsecT e s m a -> ParsecT e s m a
start s p = s >> p

stop :: (ErrorComponent e, Stream s, Token s ~ Char) => 
  ParsecT e s m b -> ParsecT e s m a -> ParsecT e s m a
stop s p = do
  notFollowedBy s
  p

end :: (ErrorComponent e, Stream s, Token s ~ Char) => 
  ParsecT e s m b -> ParsecT e s m a -> ParsecT e s m a
end s p = do
  p' <- stop s p
  void s
  return p'

sep :: (ErrorComponent e, Stream s, Token s ~ Char) =>
  ParsecT e s m b -> ParsecT e s m a -> ParsecT e s m [a]
sep s p = many $ stop s $ do
  p' <- p
  void $ optional s
  return p'

sep1 :: (ErrorComponent e, Stream s, Token s ~ Char) =>
  ParsecT e s m b -> ParsecT e s m a -> ParsecT e s m [a]
sep1 s p = some $ stop s $ do
  p' <- p
  void $ optional s
  return p'

twin :: (ErrorComponent e, Stream s, Token s ~ Char) => 
  ParsecT e s m c -> 
  ParsecT e s m a ->
  ParsecT e s m b ->
  ParsecT e s m (a, b)
twin t p q = do
  p' <- end t p
  q' <- q
  return (p', q')

tween :: (ErrorComponent e, Stream s, Token s ~ Char) => 
  ParsecT e s m b -> ParsecT e s m c -> ParsecT e s m a -> ParsecT e s m a
tween s e p = start s $ end e p

someTween :: (ErrorComponent e, Stream s, Token s ~ Char) => 
  ParsecT e s m b -> ParsecT e s m c -> ParsecT e s m a -> ParsecT e s m [a]
someTween s e p = do
  st <- start s $ some $ stop e p
  void e
  return st

manyTween :: (ErrorComponent e, Stream s, Token s ~ Char) => 
  ParsecT e s m b -> ParsecT e s m c -> ParsecT e s m a -> ParsecT e s m [a]
manyTween s e p = do
  mt <- start s $ many $ stop e p
  void e
  return mt

oneOfS :: (ErrorComponent e, Stream s, Token s ~ Char) => 
  [String] -> ParsecT e s m String
oneOfS s = choice $ string <$> s

line :: (ErrorComponent e, Stream s, Token s ~ Char) => 
  ParsecT e s m (Paragraph Line)
line = do
  l <- ils
  void newline
  return l

line' :: (ErrorComponent e, Stream s, Token s ~ Char) =>
  ParsecT e s m Text
line' = do
  l <- some $ do
    notFollowedBy newline
    printChar
  void $ newline
  return $ fromString l

specialLine :: (ErrorComponent e, Stream s, Token s ~ Char)  =>
  ParsecT e s m b -> ParsecT e s m (Paragraph Line)
specialLine s = s >> spaceChar >> line

specialLine' :: (ErrorComponent e, Stream s, Token s ~ Char)  =>
  ParsecT e s m b -> ParsecT e s m Text
specialLine' s = s >> spaceChar >> line'

nonSpacePrintChar :: (ErrorComponent e, Stream s, Token s ~ Char) => ParsecT e s m Char
nonSpacePrintChar = satisfy (\c -> (not $ isSpace c) && isPrint c)

optionalAttrMap :: (ErrorComponent e, Stream s, Token s ~ Char) => ParsecT e s m AttrMap
optionalAttrMap = do
  mam <- optional $ TM.try $ do
    am <- ils
    void $ optional newline
    return am
  return $ maybe (AttrMap mempty) id mam

-- gobbleSpace :: (ErrorComponent e, Stream s, Token s ~ Char) => ParsecT e s m ()
-- gobbleSpace = void $ many spaceChar

data Line

newtype IDPathComponent a = IDPathComponent Text deriving (Eq, Show, IsString, Generic)
instance GP.Out (IDPathComponent a)
instance (ErrorComponent e, Stream s, Token s ~ Char) => 
  ILS e s m (IDPathComponent SetID) where
  ils = fmap fromString $ 
        some $ 
        stop (oneOfS stops) $ 
        nonSpacePrintChar
    where
      stops = [ "/"
              , "]]"
              , "#"
              ]

instance (ErrorComponent e, Stream s, Token s ~ Char) => 
  ILS e s m (IDPathComponent Back) where
  ils = fmap fromString $ 
        some $ 
        stop (oneOfS stops) $ 
        nonSpacePrintChar
    where
      stops = [ "/"
              , ">>"
              , "#"
              ]

instance (ErrorComponent e, Stream s, Token s ~ Char) => 
  ILS e s m (IDPathComponent PrerexItem) where
  ils = fmap fromString $ 
        some $ 
        stop (oneOfS stops) $ 
        nonSpacePrintChar
    where
      stops = [ "/"
              , "\n"
              , "{"
              ]

newtype IDPath a = IDPath (Vector (IDPathComponent a)) deriving (Eq, Show, Generic)
instance GP.Out (IDPath a)
instance (ErrorComponent e, Stream s, Token s ~ Char, ILS e s m (IDPathComponent a)) => 
  ILS e s m (IDPath a) where
  ils = do
    void $ char '/'
    ipcs <- sep (char '/') ils
    return $ IDPath $ fromList ipcs

newtype IDHash a = IDHash Text deriving (Eq, Show, IsString, Generic)
instance GP.Out (IDHash a)
instance (ErrorComponent e, Stream s, Token s ~ Char) => 
  ILS e s m (IDHash SetID) where
  ils = fmap fromString $ start (char '#') $ many $ stop (oneOfS stops) $ printChar
    where
      stops = ["]]"]

instance (ErrorComponent e, Stream s, Token s ~ Char) => 
  ILS e s m (IDHash Back) where
  ils = fmap fromString $ start (char '#') $ many $ stop (oneOfS stops) $ printChar
    where
      stops = [">>"]

instance (ErrorComponent e, Stream s, Token s ~ Char) => 
  ILS e s m (IDHash Internal) where
  ils = fmap fromString $ start (char '#') $ many $ stop (oneOfS stops) $ printChar
    where
      stops = [">>"]

data ID a = ID { idPath :: IDPath a
               , idHash :: IDHash a
               } deriving (Eq, Show, Generic)
instance GP.Out (ID a)
instance ( ErrorComponent e
         , Stream s
         , Token s ~ Char
         , ILS e s m (IDPath a)
         , ILS e s m (IDHash a)) => 
  ILS e s m (ID a) where
  ils = do
    p <- ils
    h <- ils
    return $ ID p h
  
newtype SetID = SetID (IDHash SetID) deriving (Eq, Show, Generic)
instance GP.Out SetID
instance (ErrorComponent e, Stream s, Token s ~ Char) => 
  ILS e s m SetID where
  ils = do
    void $ many $ spaceChar
    fmap SetID $ tween (string "[[") (string "]]") ils

newtype AttrName = AttrName Text deriving (Eq, Show, IsString, Ord, Generic)
instance GP.Out AttrName
instance (ErrorComponent e, Stream s, Token s ~ Char) =>
  ILS e s m AttrName where
  ils = fmap fromString $ some $ stop (oneOfS stops) nonSpacePrintChar
    where
      stops = [ "="
              , ","
              , "]" ]

newtype AttrValue = AttrValue Text deriving (Eq, Show, Generic, IsString)
instance GP.Out AttrValue
instance (ErrorComponent e, Stream s, Token s ~ Char) =>
  ILS e s m AttrValue where
  ils = fmap fromString $ someTween (char '"') (char '"') printChar

newtype Attribute = Attribute (AttrName, Maybe AttrValue) deriving (Eq, Show, Generic)
instance GP.Out Attribute
instance (ErrorComponent e, Stream s, Token s ~ Char) =>
  ILS e s m Attribute where
  ils = do
    n <- ils
    s <- lookAhead $ oneOf [ '=', ',', ']' ]
    v <- case s of
           '=' -> do
             void $ char '='
             Just <$> ils
           ',' -> return Nothing
           ']' -> return Nothing
           _   -> error "This should never show -- ils :: Attribute"
    return $ Attribute (n, v)

instance (GP.Out a, GP.Out b, Ord a) => (GP.Out (Map a b)) where
  doc = GP.doc . mapToList
  docPrec n = GP.docPrec n . mapToList

newtype AttrMap = AttrMap (Map AttrName (Maybe AttrValue)) deriving (Eq, Show, Generic)
instance GP.Out AttrMap
instance (ErrorComponent e, Stream s, Token s ~ Char) =>
  ILS e s m AttrMap where
  ils = do
    void $ many $ spaceChar
    fmap attrMap $ someTween (char '[') (char ']') $ ils
    where
      attrMap xs = AttrMap $ mapFromList $ fmap (\(Attribute x) -> x) xs
    
newtype InlineMath = InlineMath Text deriving (Eq, Show, Generic, IsString)
instance GP.Out InlineMath
instance (ErrorComponent e, Stream s, Token s ~ Char) =>
  ILS e s m InlineMath where
  ils = fmap fromString $ someTween (char '$') (char '$') $ 
    (TM.try $ char '\\' >> char '$') <|> printChar <|> spaceChar

newtype CommentLine = CommentLine (Paragraph Line) deriving (Eq, Show, Generic)
instance GP.Out CommentLine
instance (ErrorComponent e, Stream s, Token s ~ Char) =>
  ILS e s m CommentLine where
  ils = fmap CommentLine $ specialLine $ string "//"

newtype Protocol = Protocol Text deriving (Eq, Show, Generic, IsString)
instance GP.Out Protocol
instance (ErrorComponent e, Stream s, Token s ~ Char) =>
  ILS e s m Protocol where
  ils = fromString <$> (choice $ [string "https", string "http"])

newtype URI = URI Text deriving (Eq, Show, Generic, IsString)
instance GP.Out URI
instance (ErrorComponent e, Stream s, Token s ~ Char) =>
  ILS e s m URI where
  ils = fmap fromString $ many $ stop (string ">>") printChar

newtype CommonLink = CommonLink (Protocol, URI) deriving (Eq, Show, Generic)
instance GP.Out CommonLink
instance (ErrorComponent e, Stream s, Token s ~ Char) =>
  ILS e s m CommonLink where
  ils = fmap CommonLink $ twin (string "://") ils ils

newtype Back = Back (ID Back) deriving (Eq, Show, Generic)
instance GP.Out Back
instance (ErrorComponent e, Stream s, Token s ~ Char) =>
  ILS e s m Back where
  ils = Back <$> ils

newtype Out = Out CommonLink deriving (Eq, Show, Generic)
instance GP.Out Out
instance (ErrorComponent e, Stream s, Token s ~ Char) =>
  ILS e s m Out where
  ils = Out <$> ils

newtype Internal = Internal (IDHash Internal) deriving (Eq, Show, Generic, IsString)
instance GP.Out Internal
instance (ErrorComponent e, Stream s, Token s ~ Char) =>
  ILS e s m Internal where
  ils = Internal <$> ils

newtype LinkText = LinkText (Paragraph Markup) deriving (Eq, Show, Generic)
instance GP.Out LinkText
instance (ErrorComponent e, Stream s, Token s ~ Char) =>
  ILS e s m LinkText where
  ils = fmap LinkText $ tween (char '{') (char '}') ils

data LinkT protocol = LinkT { linkText :: Maybe LinkText
                            , linkAttrs :: AttrMap
                            , linkLocation :: protocol
                            } deriving (Eq, Show, Generic)
instance GP.Out protocol => GP.Out (LinkT protocol)
instance (ErrorComponent e, Stream s, Token s ~ Char, ILS e s m protocol) =>
  ILS e s m (LinkT protocol) where
  ils = do
    am <- optionalAttrMap
    ll <- tween (string "<<") (string ">>") ils
    lt <- optional $ TM.try ils
    return $ LinkT { linkText = lt
                   , linkAttrs = am
                   , linkLocation = ll
                   }

type BLink = LinkT Back
type OLink = LinkT Out
type ILink = LinkT Internal

data Link = LBLink BLink
          | LOLink OLink
          | LILink ILink deriving (Eq, Show, Generic)
instance GP.Out Link
instance (ErrorComponent e, Stream s, Token s ~ Char) =>
  ILS e s m Link where
  ils = (TM.try $ LILink <$> ils) <|>
        (TM.try $ LBLink <$> ils) <|>
        (         LOLink <$> ils)

data Bold
data Italic
data Monospace
data Superscript
data Subscript
data Quoted

data QTextT a style = QTextT { qtextText :: Text
                             , qtextAttrs :: AttrMap
                             } deriving (Eq, Show, Generic)
instance GP.Out (QTextT a style)

newtype LikeLine a = LikeLine Bool deriving (Eq, Show, Generic)
class LineLike ctxt where
  lineLike :: LikeLine ctxt
  lineLike = LikeLine False

instance (LineLike a, LineLike b) => LineLike (a, b) where
  lineLike = 
    let (LikeLine a :: LikeLine a) = lineLike
        (LikeLine b :: LikeLine b) = lineLike
    in
      LikeLine (a || b)

instance LineLike Main
instance LineLike Block
instance LineLike Markup where
  lineLike = LikeLine True
instance LineLike Line where
  lineLike = LikeLine True
instance LineLike Unordered where
  lineLike = LikeLine True
instance LineLike Ordered where
  lineLike = LikeLine True
instance LineLike Labelled where
  lineLike = LikeLine True
instance LineLike Label where
  lineLike = LikeLine True
instance LineLike LinkText where
  lineLike = LikeLine True

newtype HardEnder e s m a = HardEnder (ParsecT e s m ())
class HardEnd e s m a where
  hardEnd :: HardEnder e s m a
  
instance (ErrorComponent e, Stream s, Token s ~ Char) =>
  HardEnd e s m Main where
  hardEnd = HardEnder $ void $ string "\n\n"

instance (ErrorComponent e, Stream s, Token s ~ Char) =>
  HardEnd e s m Markup where
  hardEnd = HardEnder $ void $ char '}'

instance (ErrorComponent e, Stream s, Token s ~ Char) =>
  HardEnd e s m Block where
  hardEnd = HardEnder $ void $ (string "\n\n" <|> string "\n---\n")

instance (ErrorComponent e, Stream s, Token s ~ Char) =>
  HardEnd e s m Line where
  hardEnd = HardEnder $ void $ char '\n'

instance (ErrorComponent e, Stream s, Token s ~ Char) =>
  HardEnd e s m Unordered where
  hardEnd = HardEnder $ void $ string "\n- " <|> string "\n+\n"

instance (ErrorComponent e, Stream s, Token s ~ Char) =>
  HardEnd e s m Ordered where
  hardEnd = HardEnder $ void $ string "\n. " <|> string "\n+\n"

instance (ErrorComponent e, Stream s, Token s ~ Char) =>
  HardEnd e s m Labelled where
  hardEnd = HardEnder $ void $ string "\n: " <|> string "\n+\n"

data Label
instance (ErrorComponent e, Stream s, Token s ~ Char) =>
  HardEnd e s m Label where
  hardEnd = HardEnder $ void $ string ":: "

instance (ErrorComponent e, Stream s, Token s ~ Char, HardEnd e s m a, HardEnd e s m b) =>
  HardEnd e s m (a, b) where
  hardEnd = let
    (HardEnder ahe :: HardEnder e s m a) = hardEnd
    (HardEnder bhe :: HardEnder e s m b) = hardEnd
    in
    HardEnder $ (TM.try ahe) <|> bhe

newtype BadEnder e s m a = BadEnder (ParsecT e s m ())
class BadEnd e s m a where
  badEnd :: BadEnder e s m a

instance (ErrorComponent e, Stream s, Token s ~ Char) =>
  BadEnd e s m Main where
  badEnd = BadEnder empty

instance (ErrorComponent e, Stream s, Token s ~ Char) =>
  BadEnd e s m Markup where
  badEnd = BadEnder $ void $ string "\n\n"

instance (ErrorComponent e, Stream s, Token s ~ Char) =>
  BadEnd e s m Block where
  badEnd = BadEnder empty

instance (ErrorComponent e, Stream s, Token s ~ Char) =>
  BadEnd e s m Line where
  badEnd = BadEnder empty

instance (ErrorComponent e, Stream s, Token s ~ Char) =>
  BadEnd e s m Unordered where
  badEnd = BadEnder empty

instance (ErrorComponent e, Stream s, Token s ~ Char) =>
  BadEnd e s m Ordered where
  badEnd = BadEnder empty

instance (ErrorComponent e, Stream s, Token s ~ Char) =>
  BadEnd e s m Labelled where
  badEnd = BadEnder empty

instance (ErrorComponent e, Stream s, Token s ~ Char) =>
  BadEnd e s m Label where
  badEnd = BadEnder $ void $ string "\n: " <|> string "\n+\n"

instance (ErrorComponent e, Stream s, Token s ~ Char, BadEnd e s m a, BadEnd e s m b) =>
  BadEnd e s m (a, b) where
  badEnd = let
    (BadEnder abe :: BadEnder e s m a) = badEnd
    (BadEnder bbe :: BadEnder e s m b) = badEnd
    in
    BadEnder $ (TM.try abe) <|> bbe
 
possibleUnexpectedHardEnd :: (ErrorComponent e, Stream s, Token s ~ Char) =>
  ParsecT e s m b -> String -> ParsecT e s m ()
possibleUnexpectedHardEnd he ctxt = do
  mhe <- optional $ TM.try he
  case mhe of
    Nothing -> return ()
    Just _ -> unexpected $ Label $ 'h' NE.:| "ard ending inside " ++ ctxt

possibleUnexpectedBadEnd :: (ErrorComponent e, Stream s, Token s ~ Char) =>
  ParsecT e s m b -> String -> ParsecT e s m ()
possibleUnexpectedBadEnd be ctxt = do
  mbe <- optional $ TM.try be
  case mbe of
    Nothing -> return ()
    Just _ -> unexpected $ Label $ 'b' NE.:| "ad ending inside " ++ ctxt

escapedDelims :: (ErrorComponent e, Stream s, Token s ~ Char) =>
  ParsecT e s m Char
escapedDelims = char '\\' >> (oneOf ("*_`^~\"" :: String))

qtext :: forall e s m a b . (ErrorComponent e, Stream s, Token s ~ Char, HardEnd e s m a, BadEnd e s m a) =>
  ParsecT e s m Char -> ParsecT e s m (QTextT a b)
qtext delim = do
  let (HardEnder he :: HardEnder e s m a) = hardEnd
      (BadEnder be :: BadEnder e s m a) = badEnd
  am <- optionalAttrMap
  t  <- fmap fromString $ tween delim delim $ some $ stop (void delim <|> he <|> be) $ (TM.try escapedDelims <|> printChar <|> spaceChar)
  possibleUnexpectedHardEnd he "QText (i.e., bold, quoted, plain text etc.).  Did you forget to close an opening '*', for example?"
  possibleUnexpectedBadEnd be "QText (i.e., bold, quoted, plain text etc.).  Did you forget to close an opening '*', for example?"
  return $ QTextT { qtextText = t
                  , qtextAttrs = am 
                  }

instance (ErrorComponent e, Stream s, Token s ~ Char, HardEnd e s m a, BadEnd e s m a) =>
  ILS e s m (QTextT a Bold) where
  ils = qtext (char '*')

instance (ErrorComponent e, Stream s, Token s ~ Char, HardEnd e s m a, BadEnd e s m a) =>
  ILS e s m (QTextT a Italic) where
  ils = qtext (char '_')

instance (ErrorComponent e, Stream s, Token s ~ Char, HardEnd e s m a, BadEnd e s m a) =>
  ILS e s m (QTextT a Monospace) where
  ils = qtext (char '`')

instance (ErrorComponent e, Stream s, Token s ~ Char, HardEnd e s m a, BadEnd e s m a) =>
  ILS e s m (QTextT a Superscript) where
  ils = qtext (char '^')

instance (ErrorComponent e, Stream s, Token s ~ Char, HardEnd e s m a, BadEnd e s m a) =>
  ILS e s m (QTextT a Subscript) where
  ils = qtext (char '~')

instance (ErrorComponent e, Stream s, Token s ~ Char, HardEnd e s m a, BadEnd e s m a) =>
  ILS e s m (QTextT a Quoted) where
  ils = qtext (char '"')

newtype PlainText a = PlainText Text deriving (Eq, Show, Generic, IsString)
instance GP.Out (PlainText a)
instance (ErrorComponent e, Stream s, Token s ~ Char, HardEnd e s m a, BadEnd e s m a, ILS e s m (SimpleContent a)) =>
  ILS e s m (PlainText a) where
  ils = do
    let (HardEnder he :: HardEnder e s m a) = hardEnd
        (BadEnder be :: BadEnder e s m a) = badEnd
    t <- fmap fromString $ some $ do
      notFollowedBy $ (TM.try $ void $ (ils :: ParsecT e s m (SimpleContent a))) <|> TM.try he <|> be
      printChar <|> spaceChar
    possibleUnexpectedBadEnd be "Paragraph.  You probably forgot to close the delimiter in a markup, or forgot the '::' in a labelled list."
    return $ fromString t

type BoldText a = QTextT a Bold
type ItalicText a = QTextT a Italic
type MonospaceText a = QTextT a Monospace
type SuperscriptText a = QTextT a Superscript
type SubscriptText a = QTextT a Subscript
type QuotedText a = QTextT a Quoted

data QText a = QTBoldText (BoldText a)
             | QTItalicText (ItalicText a)
             | QTMonospaceText (MonospaceText a)
             | QTSuperscriptText (SuperscriptText a)
             | QTSubscriptText (SubscriptText a)
             | QTQuotedText (QuotedText a)
             deriving (Eq, Show, Generic)
instance GP.Out (QText a)

instance (ErrorComponent e, Stream s, Token s ~ Char, HardEnd e s m a, BadEnd e s m a) =>
  ILS e s m (QText a) where
  ils = (TM.try $ QTBoldText <$> ils) <|>
        (TM.try $ QTItalicText <$> ils) <|>
        (TM.try $ QTMonospaceText <$> ils) <|>
        (TM.try $ QTSuperscriptText <$> ils) <|>
        (TM.try $ QTSubscriptText <$> ils) <|>
        (         QTQuotedText <$> ils)

markup1 :: (ErrorComponent e, Stream s, Token s ~ Char, ILS e s m a) =>
  ParsecT e s m b -> ParsecT e s m a
markup1 s = do
  void s
  mu <- start (char '{') $ stop (char '}') ils
  void $ char '}'
  return mu

data Markup

data Footnote = Footnote { footnoteContent :: Paragraph Markup
                         , footnoteAttrs   :: AttrMap
                         , footnoteID      :: Maybe SetID
                         } deriving (Eq, Show, Generic)
instance GP.Out Footnote
instance (ErrorComponent e, Stream s, Token s ~ Char) =>
  ILS e s m Footnote where
  ils = do
    am <- optionalAttrMap
    fc <- markup1 (string "footnote:")
    fid <- optional $ TM.try ils
    return $ Footnote { footnoteContent = fc
                      , footnoteAttrs = am
                      , footnoteID = fid
                      }

data FootnoteRef = FootnoteRef { footnoteRefID :: ILink
                               , footnoteRefAttrs :: AttrMap
                               , footnoteRefIDID :: Maybe SetID
                               } deriving (Eq, Show, Generic)
instance GP.Out FootnoteRef
instance (ErrorComponent e, Stream s, Token s ~ Char) =>
  ILS e s m FootnoteRef where
  ils = do
    am <- optionalAttrMap
    frc <- markup1 (string "footnoteref:")
    frid <- optional $ TM.try ils
    return FootnoteRef { footnoteRefID = frc
                       , footnoteRefAttrs = am
                       , footnoteRefIDID = frid
                       }

data SimpleContent a = SCInlineMath InlineMath
                     | SCLink Link
                     | SCFootnote Footnote
                     | SCFootnoteRef FootnoteRef
                     | SCQText (QText a) deriving (Eq, Show, Generic)
instance GP.Out (SimpleContent a)
instance (ErrorComponent e, Stream s, Token s ~ Char, HardEnd e s m a, BadEnd e s m a) =>
  ILS e s m (SimpleContent a) where
  ils = (TM.try $ SCInlineMath <$> ils) <|>
        (TM.try $ SCLink <$> ils) <|>
        (TM.try $ SCFootnote <$> ils) <|>
        (TM.try $ SCFootnoteRef <$> ils) <|>
        (         SCQText <$> ils)

data ParagraphContent a = PCSimpleContent (SimpleContent a)
                        | PCPlainText (PlainText a) deriving (Eq, Show, Generic)
instance GP.Out (ParagraphContent a)
instance (ErrorComponent e, Stream s, Token s ~ Char, HardEnd e s m a, BadEnd e s m a) =>
  ILS e s m (ParagraphContent a) where
  ils = (TM.try $ PCSimpleContent <$> ils) <|> 
        (         PCPlainText <$> ils)

data Paragraph a = Paragraph { paragraphContent :: Vector (ParagraphContent a)
                             , paragraphID :: Maybe SetID
                             } deriving (Eq, Show, Generic)
instance GP.Out (Paragraph a)

instance (ErrorComponent e, Stream s, Token s ~ Char, HardEnd e s m a, BadEnd e s m a, LineLike a) =>
  ILS e s m (Paragraph a) where
  ils = do
    let (LikeLine ll :: LikeLine a) = lineLike
    notFollowedBy $ TM.try $ void (ils :: ParsecT e s m (SectionHeading)) <|>
                             void (ils :: ParsecT e s m (SubsectionHeading))
    void $ many $ spaceChar
    scs <- some ils
    pid <- if ll then
             return Nothing
           else
             optional $ TM.try ils
    return $ Paragraph { paragraphContent = fromList scs
                       , paragraphID = pid
                       }

data Unordered = Unordered deriving (Eq, Show, Generic)
instance GP.Out Unordered
instance (ErrorComponent e, Stream s, Token s ~ Char) => 
  ILS e s m Unordered where
  ils = return Unordered

data Ordered = Ordered deriving (Eq, Show, Generic)
instance GP.Out Ordered
instance (ErrorComponent e, Stream s, Token s ~ Char) => 
  ILS e s m Ordered where
  ils = return Ordered

newtype Labelled = Labelled (Paragraph Label) deriving (Eq, Show, Generic)
instance GP.Out Labelled
instance (ErrorComponent e, Stream s, Token s ~ Char) => 
  ILS e s m Labelled where
  ils = Labelled <$> do
    l <- ils
    void $ string "::" >> spaceChar
    return l

listItem :: forall e s m labelType a . (ErrorComponent e, Stream s, Token s ~ Char, HardEnd e s m (a, labelType), BadEnd e s m (a, labelType), ILS e s m labelType, LineLike labelType, LineLike a) => 
  Char -> ParsecT e s m (ListItem a labelType)
listItem s = do
  void $ char s >> spaceChar
  lil <- ils
  lic <- ils
  return $ ListItem { listItemContents = lic
                    , listItemLabel = lil
                    }

data ListItem a labelType = ListItem { listItemContents :: Paragraph (a, labelType)
                                     , listItemLabel    :: labelType
                                     } deriving (Eq, Show, Generic)
instance GP.Out labelType => GP.Out (ListItem a labelType)
instance (ErrorComponent e, Stream s, Token s ~ Char, HardEnd e s m a, BadEnd e s m a, LineLike a) => 
  ILS e s m (ListItem a Unordered) where
  ils = listItem '-'

instance (ErrorComponent e, Stream s, Token s ~ Char, HardEnd e s m a, BadEnd e s m a, LineLike a) => 
  ILS e s m (ListItem a Ordered) where
  ils = listItem '.'

instance (ErrorComponent e, Stream s, Token s ~ Char, HardEnd e s m a, BadEnd e s m a, LineLike a) => 
  ILS e s m (ListItem a Labelled) where
  ils = listItem ':'

data ListT a labelType = ListT { listItems :: Vector (ListItem a labelType)
                               , listAttrs :: AttrMap
                               } deriving (Eq, Show, Generic)
instance GP.Out labelType => GP.Out (ListT a labelType)
instance (ErrorComponent e, Stream s, Token s ~ Char, ILS e s m labelType, ILS e s m (ListItem a labelType)) =>
  ILS e s m (ListT a labelType) where
  ils = do
    void $ many $ spaceChar
    am <- do
      oam <- optionalAttrMap
      return oam
    lis <- sep1 newline ils
    return $ ListT { listItems = fromList lis
                   , listAttrs = am
                   }

type UList a = ListT a Unordered
type OList a = ListT a Ordered
type LList a = ListT a Labelled
  
data List a = LUList (UList a)
            | LOList (OList a)
            | LLList (LList a) deriving (Eq, Show, Generic)
instance GP.Out (List a)
instance (ErrorComponent e, Stream s, Token s ~ Char, HardEnd e s m a, BadEnd e s m a, LineLike a) =>
  ILS e s m (List a) where
  ils = (TM.try $ LUList <$> ils) <|>
        (TM.try $ LOList <$> ils) <|>
        (         LLList <$> ils)

newtype ImageLink = ImageLink Text deriving (Eq, Show, Generic, IsString)
instance GP.Out ImageLink
newtype VideoLink = VideoLink Text deriving (Eq, Show, Generic, IsString)
instance GP.Out VideoLink
newtype YouTubeLink = YouTubeLink Text deriving (Eq, Show, Generic, IsString)
instance GP.Out YouTubeLink
instance (ErrorComponent e, Stream s, Token s ~ Char) =>
  ILS e s m YouTubeLink where
  ils = do
    space
    fmap fromString $ someTween (string "<<") (string ">>") printChar

newtype BibliographyItem = BibliographyItem Text deriving (Eq, Show, Generic, IsString)
instance GP.Out BibliographyItem
instance (ErrorComponent e, Stream s, Token s ~ Char) => 
  ILS e s m BibliographyItem where
  ils = do
    traceShowM ("WARNING: calling parseDoc :: BibliographyContent, which is not really totally implemented." :: String)
    t <- some $ do
      notFollowedBy blockDelim
      printChar
    return $ BibliographyItem $ fromString $ t

data PrerexItem = PrerexItem { prerexPath :: IDPath PrerexItem
                             , prerexDescription :: Paragraph (Markup, Block) } deriving (Eq, Show, Generic)
instance GP.Out PrerexItem
instance (ErrorComponent e, Stream s, Token s ~ Char) =>
  ILS e s m PrerexItem where
  ils = do
    space
    p' <- ils
    d <- tween (char '{') (char '}') ils
    return $ PrerexItem { prerexPath = p'
                        , prerexDescription = d
                        }

newtype BlockType a = BlockType Text deriving (Eq, Show, Generic, IsString)
instance GP.Out  (BlockType a)
instance (ErrorComponent e, Stream s, Token s ~ Char) => 
  ILS e s m (BlockType a) where
  ils = fmap BlockType $ specialLine' $ char '@'

class TypedBlock a where
  bType :: BlockType a

class ILSBlock e s m a where
  ilsBlock :: Maybe BlockHeading -> AttrMap -> ParsecT e s m (BlockT a)

blockStart :: (ErrorComponent e, Stream s, Token s ~ Char) =>
  ParsecT e s m String
blockStart = string "---\n"

blockDelim :: (ErrorComponent e, Stream s, Token s ~ Char) =>
  ParsecT e s m String
blockDelim = string "\n---\n"

simpleBlock :: (ErrorComponent e, Stream s, Token s ~ Char) => 
  (a -> b) -> ParsecT e s m a -> Maybe BlockHeading -> AttrMap -> ParsecT e s m (BlockT b)
simpleBlock f p bt am = do
  c <- tween blockStart blockDelim p
  bid <- optional $ TM.try ils
  return $ BlockT { blockContents = f c
                  , blockAttrs = am
                  , blockTitle = bt
                  , blockID = bid
                  }

newtype Prerex = Prerex (Vector PrerexItem) deriving (Eq, Show, Generic)
instance GP.Out Prerex
instance TypedBlock Prerex where bType = "prerex"
instance (ErrorComponent e, Stream s, Token s ~ Char) => 
  ILSBlock e s m Prerex where
  ilsBlock = simpleBlock (Prerex . fromList) $ some $ do 
    notFollowedBy blockDelim
    ils

newtype Introduction = Introduction (AnonymousSection Block) deriving (Eq, Show, Generic)
instance GP.Out Introduction
instance TypedBlock Introduction where bType = "introduction"
instance (ErrorComponent e, Stream s, Token s ~ Char) => 
  ILSBlock e s m Introduction where
  ilsBlock = simpleBlock Introduction ils

blockText :: (ErrorComponent e, Stream s, Token s ~ Char) => 
  ParsecT e s m String
blockText = some $ do
  notFollowedBy blockDelim
  printChar <|> spaceChar
  
newtype Math = Math Text deriving (Eq, Show, Generic, IsString)
instance GP.Out Math
instance TypedBlock Math where bType = "math"
instance (ErrorComponent e, Stream s, Token s ~ Char) => 
  ILSBlock e s m Math where
  ilsBlock = simpleBlock fromString blockText 

newtype Equation = Equation Text deriving (Eq, Show, Generic, IsString)
instance GP.Out Equation

newtype EqnArray = EqnArray (Vector Equation) deriving (Eq, Show, Generic)
instance GP.Out EqnArray
instance TypedBlock EqnArray where bType = "eqnarray"
instance (ErrorComponent e, Stream s, Token s ~ Char) => 
  ILSBlock e s m EqnArray where
  ilsBlock = simpleBlock (EqnArray . fromList . fmap fromString . lines) $ blockText

newtype Theorem = Theorem (AnonymousSection Block) deriving (Eq, Show, Generic)
instance GP.Out Theorem
instance TypedBlock Theorem where bType = "theorem"
instance (ErrorComponent e, Stream s, Token s ~ Char) => 
  ILSBlock e s m Theorem where
  ilsBlock = simpleBlock Theorem ils

newtype Lemma = Lemma (AnonymousSection Block) deriving (Eq, Show, Generic)
instance GP.Out Lemma
instance TypedBlock Lemma where bType = "lemma"
instance (ErrorComponent e, Stream s, Token s ~ Char) => 
  ILSBlock e s m Lemma where
  ilsBlock = simpleBlock Lemma ils

newtype Corollary = Corollary (AnonymousSection Block) deriving (Eq, Show, Generic)
instance GP.Out Corollary
instance TypedBlock Corollary where bType = "corollary"
instance (ErrorComponent e, Stream s, Token s ~ Char) => 
  ILSBlock e s m Corollary where
  ilsBlock = simpleBlock Corollary ils

newtype Proposition = Proposition (AnonymousSection Block) deriving (Eq, Show, Generic)
instance GP.Out Proposition
instance TypedBlock Proposition where bType = "proposition"
instance (ErrorComponent e, Stream s, Token s ~ Char) => 
  ILSBlock e s m Proposition where
  ilsBlock = simpleBlock Proposition ils

newtype Conjecture = Conjecture (AnonymousSection Block) deriving (Eq, Show, Generic)
instance GP.Out Conjecture
instance TypedBlock Conjecture where bType = "conjecture"
instance (ErrorComponent e, Stream s, Token s ~ Char) => 
  ILSBlock e s m Conjecture where
  ilsBlock = simpleBlock Conjecture ils

newtype Axiom = Axiom (AnonymousSection Block) deriving (Eq, Show, Generic)
instance GP.Out Axiom
instance TypedBlock Axiom where bType = "axiom"
instance (ErrorComponent e, Stream s, Token s ~ Char) =>
  ILSBlock e s m Axiom where
  ilsBlock = simpleBlock Axiom ils

newtype Proof = Proof (AnonymousSection Block) deriving (Eq, Show, Generic)
instance GP.Out Proof
instance TypedBlock Proof where bType = "proof"
instance (ErrorComponent e, Stream s, Token s ~ Char) => 
  ILSBlock e s m Proof where
  ilsBlock = simpleBlock Proof ils

newtype Quote = Quote (Paragraph Block) deriving (Eq, Show, Generic)
instance GP.Out Quote
instance TypedBlock Quote where bType = "quote"
instance (ErrorComponent e, Stream s, Token s ~ Char) => 
  ILSBlock e s m Quote where
  ilsBlock = simpleBlock Quote ils

newtype CodeLine = CodeLine Text deriving (Eq, Show, IsString, Generic)
instance GP.Out CodeLine

newtype Code = Code (Vector CodeLine) deriving (Eq, Show, Generic)
instance GP.Out Code
instance TypedBlock Code where bType = "code"
instance (ErrorComponent e, Stream s, Token s ~ Char) => 
  ILSBlock e s m Code where
  ilsBlock = simpleBlock (Code . fromList) $ 
             (fmap (\x -> fmap fromString x)) $ 
             lines <$> codeLines
    where
      codeLines = fmap fromString $ some $ do
        notFollowedBy (blockDelim :: ParsecT e s m String)
        printChar <|> spaceChar

newtype Image = Image OLink deriving (Eq, Show, Generic)
instance GP.Out Image
instance TypedBlock Image where bType = "image"
instance (ErrorComponent e, Stream s, Token s ~ Char) => 
  ILSBlock e s m Image where
  ilsBlock = simpleBlock Image ils

newtype Video = Video OLink deriving (Eq, Show, Generic)
instance GP.Out Video
instance TypedBlock Video where bType = "video"
instance (ErrorComponent e, Stream s, Token s ~ Char) => 
  ILSBlock e s m Video where
  ilsBlock = simpleBlock Video ils

data YouTube = YouTube YouTubeLink deriving (Eq, Show, Generic)
instance GP.Out YouTube
instance TypedBlock YouTube where bType = "youtube"
instance (ErrorComponent e, Stream s, Token s ~ Char) => 
  ILSBlock e s m YouTube where
  ilsBlock = simpleBlock YouTube ils

data Connection = Connection { connectionPrerex :: Maybe PrerexBlock
                             , connectionContents :: AnonymousSection Block } deriving (Eq, Show, Generic)
instance GP.Out Connection
instance TypedBlock Connection where bType = "connection"
instance (ErrorComponent e, Stream s, Token s ~ Char) => 
  ILSBlock e s m Connection where
  ilsBlock = simpleBlock (\(pb, c) -> Connection pb c) $ do
    pb <- optional $ TM.try ils
    c <- ils
    return (pb, c)

newtype Definition = Definition (AnonymousSection Block) deriving (Eq, Show, Generic)
instance GP.Out Definition
instance TypedBlock Definition where bType = "definition"
instance (ErrorComponent e, Stream s, Token s ~ Char) => 
  ILSBlock e s m Definition where
  ilsBlock = simpleBlock Definition ils

newtype Intuition = Intuition (AnonymousSection Block) deriving (Eq, Show, Generic)
instance GP.Out Intuition
instance TypedBlock Intuition where bType = "intuition"
instance (ErrorComponent e, Stream s, Token s ~ Char) => 
  ILSBlock e s m Intuition where
  ilsBlock = simpleBlock Intuition ils

newtype Admonition = Admonition (Paragraph Block) deriving (Eq, Show, Generic)
instance GP.Out Admonition
instance TypedBlock Admonition where bType = "admonition"
instance (ErrorComponent e, Stream s, Token s ~ Char) => 
  ILSBlock e s m Admonition where
  ilsBlock = simpleBlock Admonition ils

newtype Sidenote = Sidenote (Paragraph Block) deriving (Eq, Show, Generic)
instance GP.Out Sidenote
instance TypedBlock Sidenote where bType = "sidenote"
instance (ErrorComponent e, Stream s, Token s ~ Char) => 
  ILSBlock e s m Sidenote where
  ilsBlock = simpleBlock Sidenote ils

newtype Example = Example (AnonymousSection Block) deriving (Eq, Show, Generic)
instance GP.Out Example
instance TypedBlock Example where bType = "example"
instance (ErrorComponent e, Stream s, Token s ~ Char) => 
  ILSBlock e s m Example where
  ilsBlock = simpleBlock Example ils

newtype Exercise = Exercise (AnonymousSection Block) deriving (Eq, Show, Generic)
instance GP.Out Exercise
instance TypedBlock Exercise where bType = "exercise"
instance (ErrorComponent e, Stream s, Token s ~ Char) => 
  ILSBlock e s m Exercise where
  ilsBlock = simpleBlock Exercise ils

newtype Bibliography = Bibliography (Vector BibliographyItem) deriving (Eq, Show, Generic)
instance GP.Out Bibliography
instance TypedBlock Bibliography where bType = "bibliography"
instance (ErrorComponent e, Stream s, Token s ~ Char) => 
  ILSBlock e s m Bibliography where
  ilsBlock = simpleBlock (Bibliography . fromList) $ many $ stop blockDelim ils

newtype FurtherReading = FurtherReading (AnonymousSection Block) deriving (Eq, Show, Generic)
instance GP.Out FurtherReading
instance TypedBlock FurtherReading where bType = "furtherreading"
instance (ErrorComponent e, Stream s, Token s ~ Char) => 
  ILSBlock e s m FurtherReading where
  ilsBlock = simpleBlock FurtherReading ils

newtype Summary = Summary (AnonymousSection Block) deriving (Eq, Show, Generic)
instance GP.Out Summary
instance TypedBlock Summary where bType = "summary"
instance (ErrorComponent e, Stream s, Token s ~ Char) => 
  ILSBlock e s m Summary where
  ilsBlock = simpleBlock Summary ils

data Recall = Recall { recallContents :: AnonymousSection Block
                     , recallLink :: BLink } deriving (Eq, Show, Generic)
instance GP.Out Recall
instance TypedBlock Recall where bType = "recall"
instance (ErrorComponent e, Stream s, Token s ~ Char) => 
  ILSBlock e s m Recall where
  ilsBlock = simpleBlock (uncurry Recall) $ do
    l <- ils
    void newline
    c <- ils
    return (l, c)

data BlockT blockType = BlockT { blockContents :: blockType
                               , blockTitle :: Maybe BlockHeading
                               , blockAttrs :: AttrMap
                               , blockID :: Maybe SetID
                               } deriving (Eq, Show, Generic)
instance GP.Out blockType => GP.Out (BlockT blockType)
instance (ErrorComponent e, Stream s, Token s ~ Char, ILSBlock e s m blockType, TypedBlock blockType) =>
  ILS e s m (BlockT blockType) where
  ils = do
    void $ many $ spaceChar
    bty <- ils :: ParsecT e s m (BlockType blockType)
    guard $ bty == blockType (error "error from ils :: BlockT: when guarding for blocktype." :: BlockT blockType)
    bt <- optional $ TM.try ils
    am <- optionalAttrMap
    ilsBlock bt am

blockType :: TypedBlock b => BlockT b -> BlockType b
blockType = const bType

type PrerexBlock = BlockT Prerex
type IntroductionBlock = BlockT Introduction
type MathBlock = BlockT Math
type EqnArrayBlock = BlockT EqnArray
type TheoremBlock = BlockT Theorem
type LemmaBlock = BlockT Lemma
type CorollaryBlock = BlockT Corollary
type PropositionBlock = BlockT Proposition
type ConjectureBlock = BlockT Conjecture
type AxiomBlock = BlockT Axiom
type ProofBlock = BlockT Proof
type QuoteBlock = BlockT Quote
type CodeBlock = BlockT Code
type ImageBlock = BlockT Image
type VideoBlock = BlockT Video
type YouTubeBlock = BlockT YouTube
type ConnectionBlock = BlockT Connection
type DefinitionBlock = BlockT Definition
type IntuitionBlock = BlockT Intuition
type AdmonitionBlock = BlockT Admonition
type SidenoteBlock = BlockT Sidenote
type ExampleBlock = BlockT Example
type ExerciseBlock = BlockT Exercise
type BibliographyBlock = BlockT Bibliography
type FurtherReadingBlock = BlockT FurtherReading
type SummaryBlock = BlockT Summary
type RecallBlock = BlockT Recall

data Block = BPrerexBlock PrerexBlock
           | BIntroductionBlock IntroductionBlock
           | BMathBlock MathBlock
           | BEqnArrayBlock EqnArrayBlock
           | BTheoremBlock TheoremBlock
           | BLemmaBlock LemmaBlock
           | BCorollaryBlock CorollaryBlock
           | BPropositionBlock PropositionBlock
           | BConjectureBlock ConjectureBlock
           | BAxiomBlock AxiomBlock
           | BProofBlock ProofBlock
           | BQuoteBlock QuoteBlock
           | BCodeBlock CodeBlock
           | BImageBlock ImageBlock
           | BVideoBlock VideoBlock
           | BYouTubeBlock YouTubeBlock
           | BConnectionBlock ConnectionBlock
           | BDefinitionBlock DefinitionBlock
           | BIntuitionBlock IntuitionBlock
           | BAdmonitionBlock AdmonitionBlock
           | BSidenoteBlock SidenoteBlock
           | BExampleBlock ExampleBlock
           | BExerciseBlock ExerciseBlock
           | BBibliographyBlock BibliographyBlock
           | BFurtherReadingBlock FurtherReadingBlock
           | BSummaryBlock SummaryBlock
           | BRecallBlock RecallBlock deriving (Eq, Show, Generic)
instance GP.Out Block
instance (ErrorComponent e, Stream s, Token s ~ Char) =>
  ILS e s m Block where
  ils = (TM.try $ BPrerexBlock <$> ils) <|>
        (TM.try $ BIntroductionBlock <$> ils) <|>
        (TM.try $ BMathBlock <$> ils) <|>
        (TM.try $ BEqnArrayBlock<$> ils) <|>
        (TM.try $ BTheoremBlock <$> ils) <|>
        (TM.try $ BLemmaBlock <$> ils) <|>
        (TM.try $ BCorollaryBlock <$> ils) <|>
        (TM.try $ BPropositionBlock <$> ils) <|>
        (TM.try $ BConjectureBlock <$> ils) <|>
        (TM.try $ BAxiomBlock <$> ils) <|>
        (TM.try $ BProofBlock <$> ils) <|>
        (TM.try $ BQuoteBlock <$> ils) <|>
        (TM.try $ BCodeBlock <$> ils) <|>
        (TM.try $ BImageBlock <$> ils) <|>
        (TM.try $ BVideoBlock <$> ils) <|>
        (TM.try $ BYouTubeBlock <$> ils) <|>
        (TM.try $ BConnectionBlock <$> ils) <|>
        (TM.try $ BDefinitionBlock <$> ils) <|>
        (TM.try $ BIntuitionBlock <$> ils) <|>
        (TM.try $ BAdmonitionBlock <$> ils) <|>
        (TM.try $ BSidenoteBlock <$> ils) <|>
        (TM.try $ BExampleBlock <$> ils) <|>
        (TM.try $ BExerciseBlock <$> ils) <|>
        (TM.try $ BBibliographyBlock <$> ils) <|>
        (TM.try $ BFurtherReadingBlock <$> ils) <|>
        (TM.try $ BSummaryBlock <$> ils) <|>
        (         BRecallBlock <$> ils)

data Section a = Section { sectionTitle :: SectionHeading
                         , sectionContents :: (Maybe (AnonymousSection a), Vector (Subsection a))
                         , sectionID :: Maybe SetID
                         } deriving (Eq, Show, Generic)
instance GP.Out (Section a)
instance (ErrorComponent e, Stream s, Token s ~ Char, HardEnd e s m a, BadEnd e s m a, SectionEnd e s m a, LineLike a) =>
  ILS e s m (Section a) where
  ils = do
    st <- ils
    sid <- optional $ TM.try ils
    manon <- optional $ TM.try ils
    sc <- many $ TM.try ils
    return $ Section { sectionTitle = st
                     , sectionContents = (manon, fromList sc)
                     , sectionID = sid
                     }
    
-- instance Line Section where lineStarter = "=="
-- instance Textlike Section where fromText = Section

data Subsection a = Subsection { subsectionTitle :: SubsectionHeading
                               , subsectionContents :: AnonymousSection a 
                               , subsectionID :: Maybe SetID
                               } deriving (Eq, Show, Generic)
instance GP.Out (Subsection a)
instance (ErrorComponent e, Stream s, Token s ~ Char, HardEnd e s m a, BadEnd e s m a, SectionEnd e s m a, LineLike a) =>
  ILS e s m (Subsection a) where
  ils = do
    st <- ils
    sid <- optional $ TM.try ils
    sc <- TM.try ils
    return $ Subsection { subsectionTitle = st
                        , subsectionContents = sc
                        , subsectionID = sid
                        }

newtype SectionEnder e s m a = SectionEnder (ParsecT e s m ())
class (ErrorComponent e, Stream s, Token s ~ Char) => SectionEnd e s m a where
  sectionEnd :: SectionEnder e s m a
  sectionEnd = SectionEnder empty

instance (ErrorComponent e, Stream s, Token s ~ Char) => SectionEnd e s m Main
instance (ErrorComponent e, Stream s, Token s ~ Char) => SectionEnd e s m Block where
  sectionEnd = SectionEnder (void $ string "\n---\n")

newtype AnonymousSection a = AnonymousSection (Vector (ComplexContent a)) deriving (Eq, Show, Generic)
instance GP.Out (AnonymousSection a)
instance (ErrorComponent e, Stream s, Token s ~ Char, HardEnd e s m a, BadEnd e s m a, SectionEnd e s m a, LineLike a) =>
  ILS e s m (AnonymousSection a) where
  ils = do
    let (SectionEnder se :: SectionEnder e s m a) = sectionEnd
    void $ many $ spaceChar
    fmap (AnonymousSection . fromList) $ some $ do
      notFollowedBy ((TM.try $ void $ (ils :: ParsecT e s m (SectionHeading))) <|>
                     (TM.try $ void $ (ils :: ParsecT e s m (SubsectionHeading))) <|>
                     se)
      ils

heading :: (ErrorComponent e, Stream s, Token s ~ Char) =>
  ParsecT e s m b -> (Paragraph Line -> a) -> ParsecT e s m a
heading s f = do
  void $ many spaceChar
  fmap f $ specialLine s

newtype DocHeading = DocHeading (Paragraph Line) deriving (Eq, Show, Generic)
instance GP.Out DocHeading
instance (ErrorComponent e, Stream s, Token s ~ Char) =>
  ILS e s m DocHeading where
  ils = heading (string "=") DocHeading
    
newtype SectionHeading = SectionHeading (Paragraph Line) deriving (Eq, Show, Generic)
instance GP.Out SectionHeading
instance (ErrorComponent e, Stream s, Token s ~ Char) =>
  ILS e s m SectionHeading where
  ils = heading (string "==") SectionHeading

newtype SubsectionHeading = SubsectionHeading (Paragraph Line) deriving (Eq, Show, Generic)
instance GP.Out SubsectionHeading
instance (ErrorComponent e, Stream s, Token s ~ Char) =>
  ILS e s m SubsectionHeading where
  ils = heading (string "===") SubsectionHeading

newtype BlockHeading = BlockHeading (Paragraph Line) deriving (Eq, Show, Generic)
instance GP.Out BlockHeading
instance (ErrorComponent e, Stream s, Token s ~ Char) =>
  ILS e s m BlockHeading where
  ils = heading (string "#") BlockHeading

data ComplexContent a = CCBlock Block
                      | CCList (List a)
                      | CCParagraph (Paragraph a) deriving (Eq, Show, Generic)
instance GP.Out (ComplexContent a)
instance (ErrorComponent e, Stream s, Token s ~ Char, HardEnd e s m a, BadEnd e s m a, LineLike a) =>
  ILS e s m (ComplexContent a) where
  ils = (TM.try $ CCBlock <$> ils) <|>
        (TM.try $ CCList <$> ils) <|>
        (         CCParagraph <$> ils)

data Content a = TLCSubsection (Subsection a)
               | TLCSection (Section a)
               | TLCAnonymousSection (AnonymousSection a) deriving (Eq, Show, Generic)
instance GP.Out (Content a)
instance (ErrorComponent e, Stream s, Token s ~ Char, HardEnd e s m a, BadEnd e s m a, SectionEnd e s m a, LineLike a) =>
  ILS e s m (Content a) where
  ils = (TM.try $ TLCSection <$> ils) <|>
        (TM.try $ TLCSubsection <$> ils) <|>
        (         TLCAnonymousSection <$> ils)

data Main

data Doc a = Doc { docTitle :: DocHeading
                 , docPrerex :: Maybe PrerexBlock
                 , docContents :: Vector (Content a)
                 } deriving (Eq, Show, Generic)
instance GP.Out (Doc a)
instance (ErrorComponent e, Stream s, Token s ~ Char, HardEnd e s m a, BadEnd e s m a, SectionEnd e s m a, LineLike a) =>
  ILS e s m (Doc a) where
  ils = do
    dt <- ils
    dp <- optional $ TM.try ils
    -- traceShow, bGenericM dp
    dc <- some $ ils
    return $ Doc { docTitle = dt
                 , docContents = fromList dc
                 , docPrerex = dp
                 }
