{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
-- | Parse2.hs
-- Author: Matt Walker
-- License: https://opensource.org/licenses/BSD-2-Clause
-- Created: Aug 25, 2017
-- Summary: 
module Text.IDoc.Parse where

import ClassyPrelude as CP

import qualified Data.List.NonEmpty as NE
import qualified Data.Set as Set
import qualified Data.Map as M
import qualified Data.Vector as V

import qualified Text.Megaparsec as MP
import Text.Megaparsec.Prim as Prim
import Text.Megaparsec.Pos

import qualified Text.IDoc.Syntax as S

import System.IO
import Data.Text.Encoding as E
import Text.Megaparsec.Text
import Text.IDoc.Lex
import qualified IPPrint.Colored as C

type IDocParseError = MP.ParseError S.Token MP.Dec

type IDocParser = MP.Parsec MP.Dec (Vector S.Token)

instance Ord s => Stream (Vector s) where
  type Token (Vector s) = s
  uncons = CP.uncons
  updatePos _ _ sp _ = (sp, sp { sourceColumn = unsafePos $ (unPos $ sourceColumn sp) + 1 })

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
    notFollowedBy r
    x
  void r
  return xs

someTween :: IDocParser l -> IDocParser r -> IDocParser a -> IDocParser (Vector a)
someTween l r x = do
  void l
  xs <- someV $ do
    notFollowedBy r
    x
  void r
  return xs

someTill :: IDocParser ender -> IDocParser a -> IDocParser (Vector a)
someTill e x = do
  xs <- someV $ do
    notFollowedBy e
    x
  void e
  return xs

manyTill :: IDocParser ender -> IDocParser a -> IDocParser (Vector a)
manyTill e x = do
  xs <- manyV $ do
    notFollowedBy e
    x
  void e
  return xs

satisfy :: (MonadParsec e s m, Token s ~ S.Token) => (S.Token -> Bool) -> m S.Token
satisfy f = Prim.token test Nothing
  where
    test x =
      if f x then
        Right x
      else
        Left (Set.singleton (MP.Tokens (x NE.:| [])), mempty, mempty)

tokenP :: S.Token -> IDocParser S.Token
tokenP t = satisfy (== t) <?> (show t)

anyTokenP :: IDocParser S.Token
anyTokenP = satisfy (\x -> x == x) <?> "Any Token"

newlineP :: IDocParser ()
newlineP = void $ tokenP S.Newline

dashP :: IDocParser ()
dashP = void $ tokenP S.Dash

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
textP = unToken <$> anyTokenP

unToken :: S.Token -> Text
unToken (S.TextT x) = x
unToken S.Equals = "="
unToken S.LAngle = "<"
unToken S.RAngle = ">"
unToken S.LBracket = "["
unToken S.RBracket = "]"
unToken S.LBrace = "{"
unToken S.RBrace = "}"
unToken S.Colon = ":"
unToken S.Newline = "\n"
unToken S.Dash = "-"
unToken S.AtSign = "@"
unToken S.BackTick = "`"
unToken S.Asterisk = "*"
unToken S.Underscore = "_"
unToken S.Octothorpe = "#"
unToken S.DoubleQuote = "\""
unToken S.Tilde = "~"
unToken S.Caret = "^"
unToken S.FSlash = "/"
unToken S.Comma = ","
unToken S.Period = "."
unToken S.DollarSign = "$"
unToken S.PercentSign = "%"
unToken S.SemiColon = ";"
unToken S.BSlash = "\\"

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
attrMapP = label "An AttrMap" $ do
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
optionalAttrMapP = label "An Optional AttrMap" $ do
  am <- optional attrMapP
  case am of
    Nothing -> return $ S.AttrMap M.empty
    Just am' -> return $ am'

-- | SetID
setIDP :: IDocParser S.SetID
setIDP = label "A SetID" $ do
  lBracketP >> lBracketP
  h <- idHashP
  rBracketP >> rBracketP
  return $ S.SetID { S._sidName = h }
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
  x <- satisfy isHttp
  colonP
  fSlashP
  return $ S.Protocol $ unToken x
  where
    isHttp (S.TextT x) | x == "https" = True
                       | x == "http"  = True
                       | otherwise = False
    isHttp _ = False
    colonP = void $ tokenP S.Colon

idBaseP :: IDocParser S.IDBase
idBaseP = S.IDBase <$> do
  fSlashP
  concat <$> (manyV $ do
                 notFollowedBy $ MP.eitherP (MP.try fSlashP) $
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
                 notFollowedBy $ MP.eitherP (MP.try rBracketP) rAngleP
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
linkP = label "A Link" $ do
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
  am <- optionalAttrMapP
  starterP
  lbl <- if hasLabel then
           Just <$> (someTill doubleStarterP simpleCoreP)
         else
           return Nothing
  cnt <- someV $ do
    notFollowedBy $ newlineP >> some newlineP
    simpleCoreP
  return $ S.ListItem { S._liAttrs = am
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
unorderedItemP = mkListItemP S.Dash False S.Unordered <?> "An Unordered List Item"

orderedItemP :: IDocParser S.ListItem
orderedItemP = mkListItemP S.Period False S.Ordered <?> "An Ordered List Item"

labelledItemP :: IDocParser S.ListItem
labelledItemP = mkListItemP S.Colon True S.Labelled <?> "A Labelled List Item"

listP :: IDocParser S.List
listP = label "A List" $ S.List <$> (     MP.try (sepEndBy1V unorderedItemP (some newlineP))
                                      <|> MP.try (sepEndBy1V orderedItemP   (some newlineP))
                                      <|>        (sepEndBy1V labelledItemP  (some newlineP)))

-- | Inline Math
inlineMathP :: IDocParser S.InlineMath 
inlineMathP = label "Some Inline Math" $ do
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
markupContentsP = label "Some Text Between Braces" $ do
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
  void $ satisfy (== (S.TextT name))
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
citationP = mkMarkupP "cite" S.Citation <?> "A Citation"

footnoteP :: IDocParser S.Markup
footnoteP = label "A Footnote" $ do
  mu <- mkMarkupP "footnote" S.Footnote 
  sid <- setIDP
  return $ mu { S._muSetID = Just sid }

footnoteRefP :: IDocParser S.Markup
footnoteRefP = mkMarkupP "footenoteref" S.FootnoteRef <?> "A Footnote Reference"

markupP :: IDocParser S.Markup
markupP = label "Some Markup" $ MP.try citationP
                            <|> MP.try footnoteP
                            <|>        footnoteRefP

-- | Paragraphs
paragraphP :: IDocParser S.Paragraph
paragraphP = do
  void $ many newlineP
  cnt <- someV $ do
    notFollowedBy $ MP.eitherP (MP.try $ newlineP >> some newlineP) $
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

-- | Blocks
-- BlockTypes must parse their own block ender.
blockP :: IDocParser S.Block
blockP = label "A Block" $ do
  am <- optionalAttrMapP
  void $ optional newlineP
  tyName <- label "A Block Type Name" $ do
    atSignP
    x <- textP
    newlineP
    return x
  title <- optional $ label "A Block Title" $ do
    octothorpeP
    someTill newlineP simpleCoreP
  ty <- case tyName of
          "prerex" -> S.PrerexB <$> prerexP
          "introduction" -> S.IntroductionB <$> introductionP
          "math" -> S.MathB <$> mathP
          "eqnarray" -> S.EqnArrayB <$> eqnArrayP
          "theorem" -> S.TheoremB <$> theoremP
          "lemma" -> S.LemmaB <$> lemmaP
          "corollary" -> S.CorollaryB <$> corollaryP
          "proposition" -> S.PropositionB <$> propositionP
          "conjecture" -> S.ConjectureB <$> conjectureP
          "axiom" -> S.AxiomB <$> axiomP
          "proof" -> S.ProofB <$> proofP
          "quote" -> S.QuoteB <$> quoteP
          "code" -> S.CodeB <$> codeP
          "image" -> S.ImageB <$> imageP
          "video" -> S.VideoB <$> videoP
          "youtube" -> S.YouTubeB <$> youTubeP
          "connection" -> S.ConnectionB <$> connectionP
          "definition" -> S.DefinitionB <$> definitionP
          "intuition" -> S.IntuitionB <$> intuitionP
          "info" -> S.InfoB <$> infoP
          "tip" -> S.TipB <$> tipP
          "caution" -> S.CautionB <$> cautionP
          "warning" -> S.WarningB <$> warningP
          "sidenote" -> S.SideNoteB <$> sideNoteP
          "example" -> S.ExampleB <$> exampleP
          "exercise" -> S.ExerciseB <$> exerciseP
          "bibliography" -> S.BibliographyB <$> bibliographyP
          "furtherreading" -> S.FurtherReadingB <$> furtherReadingP
          "summary" -> S.SummaryB <$> summaryP
          "recall" -> S.RecallB <$> recallP
          x -> error $ "unknown block type `" ++ (show x) ++ "'."
  sid <- optional setIDP
  return $ S.Block { S._bType = ty
                   , S._bAttrs = am
                   , S._bTitle = S.BlockTitle <$> title
                   , S._bSetID = sid
                   }
  where
    atSignP = void $ tokenP S.AtSign

blockStarterP :: IDocParser ()
blockStarterP = do
  dashP >> dashP >> dashP >> newlineP

blockEnderP :: IDocParser ()
blockEnderP = do
  dashP >> dashP >> dashP >> newlineP

blockEnderP' :: IDocParser ()
blockEnderP' = newlineP >> blockEnderP

multipartSeparatorP :: IDocParser ()
multipartSeparatorP = do
  asteriskP >> asteriskP >> asteriskP >> newlineP

simpleCoreBlockP :: IDocParser (Vector S.SimpleCore)
simpleCoreBlockP = do
  blockStarterP
  someTill blockEnderP' simpleCoreP

coreBlockP :: IDocParser (Vector S.Core)
coreBlockP = do
  blockStarterP
  someTill (many newlineP >> blockEnderP) coreP

uninterpretedBlockP :: IDocParser (Vector S.Token)
uninterpretedBlockP = do
  blockStarterP
  xs <- someV $ do
    notFollowedBy blockEnderP'
    anyTokenP
  blockEnderP'
  return xs

linkBlockP :: IDocParser S.Link
linkBlockP = do
  blockStarterP
  x <- linkP
  blockEnderP'
  return x

biblioBlockP :: IDocParser (Vector S.BibItem)
biblioBlockP = do
  blockStarterP
  x <- undefined
  blockEnderP'
  return x

prerexItemP :: IDocParser S.PrerexItem
prerexItemP = do
  path <- idP
  desc <- markupContentsP
  return $ S.PrerexItem { S._prerexItemPath = path
                        , S._prerexItemDescription = desc
                        }

prerexP :: IDocParser S.Prerex
prerexP = S.Prerex <$> do
  blockStarterP
  someTill blockEnderP $ do
    x <- prerexItemP
    newlineP
    return x

introductionP :: IDocParser S.Introduction
introductionP = S.Introduction <$> coreBlockP

mathP :: IDocParser S.Math
mathP = S.Math <$> uninterpretedBlockP

equationP :: IDocParser S.Equation
equationP = S.Equation <$> uninterpretedBlockP

eqnArrayP :: IDocParser S.EqnArray
eqnArrayP = S.EqnArray <$> uninterpretedBlockP

theoremP :: IDocParser S.Theorem
theoremP = S.Theorem <$> coreBlockP

lemmaP :: IDocParser S.Lemma
lemmaP = S.Lemma <$> coreBlockP

corollaryP :: IDocParser S.Corollary
corollaryP = S.Corollary <$> coreBlockP

propositionP :: IDocParser S.Proposition
propositionP = S.Proposition <$> coreBlockP

conjectureP :: IDocParser S.Conjecture
conjectureP = S.Conjecture <$> coreBlockP

axiomP :: IDocParser S.Axiom
axiomP = S.Axiom <$> coreBlockP

proofP :: IDocParser S.Proof
proofP = S.Proof <$> coreBlockP

quoteP :: IDocParser S.Quote
quoteP = S.Quote <$> simpleCoreBlockP

codeP :: IDocParser S.Code
codeP = S.Code <$> uninterpretedBlockP

imageP :: IDocParser S.Image
imageP = S.Image <$> linkBlockP

videoP :: IDocParser S.Video
videoP = S.Video <$> linkBlockP

youTubeP :: IDocParser S.YouTube
youTubeP = S.YouTube <$> linkBlockP

connectionP :: IDocParser S.Connection
connectionP = S.Connection <$> coreBlockP

definitionP :: IDocParser S.Definition
definitionP = S.Definition <$> coreBlockP

intuitionP :: IDocParser S.Intuition
intuitionP = S.Intuition <$> coreBlockP

infoP :: IDocParser S.Info
infoP = S.Info <$> coreBlockP

tipP :: IDocParser S.Tip
tipP = S.Tip <$> coreBlockP

cautionP :: IDocParser S.Caution
cautionP = S.Caution <$> coreBlockP

warningP :: IDocParser S.Warning
warningP = S.Warning <$> coreBlockP

sideNoteP :: IDocParser S.SideNote
sideNoteP = S.SideNote <$> coreBlockP

exampleP :: IDocParser S.Example
exampleP = S.Example <$> coreBlockP

exerciseP :: IDocParser S.Exercise
exerciseP = S.Exercise <$> coreBlockP

bibliographyP :: IDocParser S.Bibliography
bibliographyP = S.Bibliography <$> biblioBlockP

furtherReadingP :: IDocParser S.FurtherReading
furtherReadingP = S.FurtherReading <$> coreBlockP

summaryP :: IDocParser S.Summary
summaryP = S.Summary <$> coreBlockP

recallP :: IDocParser S.Recall
recallP = S.Recall <$> coreBlockP

-- | Section
sectionP :: IDocParser S.Section
sectionP = do
  am <- optionalAttrMapP
  equalses <- some equalsP
  let ty = case length equalses of
             2 -> S.TopSection
             3 -> S.SubSection
             _ -> error "only `Section's and `Subsection's are allowed for now."
  title <- simpleLineP
  sid <- optional setIDP
  void $ many newlineP
  cnt <- manyV $ do
    MP.notFollowedBy $ (many newlineP) >> equalsP >> equalsP
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

coreP :: IDocParser S.Core
coreP = S.CC <$> ((S.BlockC     <$> (MP.try blockP))
             <|>  (S.ListC      <$> (MP.try listP))
             <|>  (S.ParagraphC <$>         paragraphP))

docP :: IDocParser S.Doc
docP = do
  title <- someTween equalsP newlineP simpleCoreP
  void $ many newlineP
  preamble <- manyV $ do
    MP.notFollowedBy $ (many newlineP) >> equalsP >> equalsP
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
                 }
  where
    equalsP = void $ tokenP S.Equals

looksLikeEofP :: IDocParser ()
looksLikeEofP = do
  void $ some newlineP

parseFileAs :: FilePath -> IDocParser a -> IO (Either IDocParseError a)
parseFileAs f ty = (withFile f ReadMode 
                    (\src -> do
                        cnts <- CP.hGetContents src
                        case MP.parse (Text.IDoc.Lex.tokens :: Parser (Vector S.Token)) f (E.decodeUtf8 cnts) of
                          (CP.Right x) -> return $ MP.parse ty "<tokens>" x
                          (CP.Left _) -> error "Could not even lex the fucking thing."))

megaMain' :: IO ()
megaMain' = (withFile "simple1.idoc" ReadMode 
             (\src -> do
                cnts <- CP.hGetContents src
                case MP.parse (Text.IDoc.Lex.tokens :: Parser (Vector S.Token)) "source.idoc" (E.decodeUtf8 cnts) of
                  (CP.Right x) -> do
                    --System.IO.hPutStr tex (Data.Text.unpack $ utRender x)
                    case MP.parse docP "<tokens>" x of
                     CP.Right y -> C.cpprint y
                     CP.Left z -> CP.print z))
  where
    equalsP = void $ tokenP S.Equals
