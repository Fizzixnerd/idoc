{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Text.IDoc.Blocks.BibTex where

import Text.Megaparsec

import Text.IDoc.Syntax
import Text.IDoc.Parse
import Text.IDoc.Render.Html5.Card
import Text.IDoc.Render.Tex

import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A

import Text.LaTeX
import Text.LaTeX.Base.Class

import Text.Printf

import Data.Data
import qualified Data.Map as M
import Data.Char
import qualified Data.Text as T

import Control.Lens

import ClassyPrelude

data BibTex = BibTex { _bibTexContents :: Vector BibEntry }
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

instance ToMarkup BibTex where
  toMarkup (BibTex b_) = concat $ intersperse br $ toMarkup <$> b_

instance MarkupMarkup m => BlockMarkup m BibTex where
  blockMarkup _ t s b_ = card
                         defaultCardOptions
                         (mTitle "Bibliography" t)
                         s
                         ""
                         Nothing
                         (toMarkup b_)

instance Markupy m => Blocky m BibTex where
  blocky _ _ _ (BibTex b_) = fileContents "refs.bib" $
                             vectorTexy b_

itemToEntry :: BibItem -> IDocParser m b BibEntry
itemToEntry bi = do
  case _itemType bi of
    ArticleET -> ArticleBE <$> (toArticle (_itemRefName bi) $ _itemFields bi)
    BookET    -> BookBE <$> (toBook (_itemRefName bi) $ _itemFields bi)
    MiscET    -> MiscBE <$> (toMisc (_itemRefName bi) $ _itemFields bi)
    _         -> fail "only supported entry types are @article, @book, and @misc right now."

data BibItem = BibItem { _itemType :: EntryType 
                       , _itemRefName :: Identifier
                       , _itemFields :: Fields }
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

-- | These functions check the Fields to make sure everything that
-- should be there is there.
check :: (Text -> a) -> Text -> Text -> Map Identifier Value -> IDocParser m b a
check c n f m = do
  let mF = M.lookup (Identifier f) m
  case mF of
    Nothing -> fail $ printf "%s missing field: %s" n f
    Just f' -> return $ c $ _unValue f'

mCheck :: (Text -> a) -> Text -> Map Identifier Value -> Maybe a
mCheck c f m = c <$> _unValue <$> (M.lookup (Identifier f) m)

eCheck :: (Text -> a) -> (Text -> b) -> Text -> Text -> Text -> Map Identifier Value -> IDocParser m b_ (Either a b)
eCheck a_ b_ n fa fb m = do
  let mFa = M.lookup (Identifier fa) m
      mFb = M.lookup (Identifier fb) m
  if isJust mFa && isJust mFb
    then fail $ printf "%s takes exactly one of: %s or %s" n fa fb
    else case mFa of
    Just fa' -> return $ Left $ a_ $ _unValue fa'
    Nothing -> case mFb of
      Just fb' -> return $ Right $ b_ $ _unValue fb'
      Nothing -> fail $ printf "%s missing field: %s or %s" n fa fb

emCheck :: (Text -> a) -> (Text -> b) -> Text -> Text -> Text -> Map Identifier Value -> IDocParser m b_ (Maybe (Either a b))
emCheck a_ b_ n fa fb m = do
  let mFa = M.lookup (Identifier fa) m
      mFb = M.lookup (Identifier fb) m
  if isJust mFa && isJust mFb
    then fail $ printf "%s takes at most one of: %s or %s" n fa fb
    else case mFa of
    Just fa' -> return $ Just $ Left $ a_ $ _unValue fa'
    Nothing -> case mFb of
      Just fb' -> return $ Just $ Right $ b_ $ _unValue fb'
      Nothing -> return $ Nothing

data Article = Article { _articleAuthor :: AuthorF
                       , _articleTitle  :: TitleF
                       , _articleJournal :: JournalF
                       , _articleYear :: YearF
                       , _articleVolume :: Maybe VolumeF
                       , _articleNumber :: Maybe NumberF
                       , _articlePages :: Maybe PagesF
                       , _articleMonth :: Maybe MonthF
                       , _articleNote :: Maybe NoteF
                       , _articleIdentifier :: Identifier}
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

instance ToMarkup Article where
  toMarkup (Article {..}) = H.div H.! class_ "idocBibEntry idocBibArticle" 
                                  H.! A.id (toValue $ _unIdentifier _articleIdentifier) $ do
    H.span H.! class_ "idocBibAuthor" $ text $ _unAuthor _articleAuthor
    text " "
    H.span H.! class_ "idocBibTitle" $ text $ "\"" ++  _unTitle _articleTitle ++ ".\""
    text " "
    i H.! class_ "idocBibJournal" $ text $ _unJournal _articleJournal ++ ","
    text " "
    maybe "" (\x -> H.span H.! class_ "idocBibVolume" $ text $ _unVolume x ++ ", ") _articleVolume
    maybe "" (\x -> H.span H.! class_ "idocBibNumber" $ text $ "no. " ++ _unNumber x ++ " ") _articleNumber
    text "("
    maybe "" (\x -> H.span H.! class_ "idocBibMonth" $ text $ _unMonth x ++ " ") _articleMonth
    H.span H.! class_ "idocBibYear" $ text $ _unYear _articleYear
    text ")"
    maybe "" (\x -> H.span H.! class_ "idocBibPages" $ text $ ":" ++ _unPages x) _articlePages
    text "."
    maybe "" (\x -> do
                 H.br
                 H.span H.! class_ "idocBibNote" $ text $ _unNote x) _articleNote

instance Texy Article where
  texy (Article {..}) = (raw $ "@article{" ++ _unIdentifier _articleIdentifier ++ ",\n") ++
                        texy _articleAuthor ++
                        texy _articleTitle ++
                        texy _articleJournal ++
                        texy _articleYear ++
                        mTexy _articleVolume ++
                        mTexy _articleNumber ++
                        mTexy _articlePages ++
                        mTexy _articleMonth ++
                        mTexy _articleNote ++
                        raw "}\n"

toArticle :: Identifier -> Fields -> IDocParser m b Article
toArticle i_ (Fields f) = do
  _articleAuthor     <- check AuthorF "article" "author" f
  _articleTitle      <- check TitleF "article" "title" f
  _articleJournal    <- check JournalF "article" "journal" f
  _articleYear       <- check YearF "article" "year" f
  let _articleVolume = mCheck VolumeF "volume" f
      _articleNumber = mCheck NumberF "number" f
      _articlePages  = mCheck PagesF  "pages"  f
      _articleMonth  = mCheck MonthF  "month"  f
      _articleNote   = mCheck NoteF   "note"   f
      _articleIdentifier = i_
  return Article {..}

data Book = Book { _bookAuthorOrEditor :: Either AuthorF EditorF
                 , _bookTitle :: TitleF
                 , _bookPublisher :: PublisherF
                 , _bookYear :: YearF
                 , _bookVolumeOrNumber :: Maybe (Either VolumeF NumberF)
                 , _bookSeries :: Maybe SeriesF
                 , _bookAddress :: Maybe AddressF
                 , _bookEdition :: Maybe EditionF
                 , _bookMonth :: Maybe MonthF
                 , _bookNote :: Maybe NoteF
                 , _bookIdentifier :: Identifier }
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

instance ToMarkup Book where
  toMarkup (Book {..}) = do
    let aore = either _unAuthor _unEditor _bookAuthorOrEditor 
    H.div H.! class_ "idocBibEntry idocBibBook"
          H.! A.id (toValue $ _unIdentifier _bookIdentifier) $ do
      H.span H.! class_ "idocBibAuthor" $ text aore
      text " "
      i H.! class_ "idocBibTitle" $ text $ _unTitle _bookTitle
      text " "
      maybe "" (\x -> H.span H.! class_ "idocBibEdition" $ text $ _unEdition x ++ " ") _bookEdition
      maybe "" (\x -> either 
                      (\vol -> H.span H.! class_ "idocBibVolume" $ text $ "Vol. " ++ _unVolume vol ++ ". ")
                      (\num -> H.span H.! class_ "idocBibNumber" $ text $ "No. " ++ _unNumber num ++ ". ")
                      x) _bookVolumeOrNumber
      maybe "" (\x -> H.span H.! class_ "idocBibSeries" $ text $ _unSeries x ++ ". ") _bookSeries
      maybe "" (\x -> H.span H.! class_ "idocBibAddress" $ text $ _unAddress x ++ ": ") _bookAddress
      H.span H.! class_ "idocBibPublisher" $ text $ _unPublisher _bookPublisher ++ ","
      text " "
      maybe "" (\x -> H.span H.! class_ "idocBibMonth" $ text $ "(" ++ _unMonth x ++ ") ") _bookMonth
      H.span H.! class_ "idocBibYear" $ text $ _unYear _bookYear
      maybe "" (\x -> do
                   br
                   H.span H.! class_ "idocBibNote" $ text $ _unNote x) _bookNote
  
instance Texy Book where
  texy (Book {..}) = (raw $ "@misc{" ++ _unIdentifier _bookIdentifier ++ ",\n") ++
                     (either texy texy _bookAuthorOrEditor) ++
                     texy _bookTitle ++
                     texy _bookPublisher ++
                     texy _bookYear ++
                     (maybe "" (either texy texy) _bookVolumeOrNumber) ++
                     mTexy _bookSeries ++
                     mTexy _bookAddress ++
                     mTexy _bookEdition ++
                     mTexy _bookMonth ++
                     mTexy _bookNote ++
                     raw "}\n"

toBook :: Identifier -> Fields -> IDocParser m b Book
toBook i_ (Fields f) = do
  _bookAuthorOrEditor <-  eCheck AuthorF EditorF "book" "author" "editor" f
  _bookTitle          <-   check TitleF          "book" "title"           f
  _bookPublisher      <-   check PublisherF      "book" "publisher"       f
  _bookYear           <-   check YearF           "book" "year"            f
  _bookVolumeOrNumber <- emCheck VolumeF NumberF "book" "volume" "number" f
  let _bookSeries     =   mCheck SeriesF                "series"          f
      _bookAddress    =   mCheck AddressF               "address"         f
      _bookEdition    =   mCheck EditionF               "edition"         f
      _bookMonth      =   mCheck MonthF                 "month"           f
      _bookNote       =   mCheck NoteF                  "note"            f
      _bookIdentifier =       i_
  return Book {..}

data Misc = Misc { _miscAuthor :: Maybe AuthorF
                 , _miscTitle :: Maybe TitleF
                 , _miscHowPublished :: Maybe HowPublishedF
                 , _miscMonth :: Maybe MonthF
                 , _miscYear :: Maybe YearF
                 , _miscNote :: Maybe NoteF 
                 , _miscIdentifier :: Identifier }
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

instance ToMarkup Misc where
  toMarkup (Misc {..}) = do
    H.div H.! class_ "idocBibEntry idocBibMisc"
          H.! A.id (toValue $ _unIdentifier _miscIdentifier) $ do
      maybe "" (\x -> H.span H.! class_ "idocBibAuthor" $ text $ _unAuthor x ++ ", ") _miscAuthor
      maybe "" (\x -> i H.! class_ "idocBibTitle" $ text $ _unTitle x ++ ". ") _miscTitle
      maybe "" (\x -> H.span H.! class_ "idocBibHowPublished" $ text $ _unHowPublished x ++ " ") _miscHowPublished
      maybe "" (\x -> H.span H.! class_ "idocBibMonth" $ text $ "(" ++  _unMonth x ++ ") ") _miscMonth
      maybe "" (\x -> H.span H.! class_ "idocBibYear" $ text $ _unYear x) _miscYear
      maybe "" (\x -> do
                   br
                   H.span H.! class_ "idocBibNote" $ text $ _unNote x) _miscNote
  
instance Texy Misc where
  texy (Misc {..}) = (raw $ "@misc{" ++ _unIdentifier _miscIdentifier ++ ",\n") ++
                     mTexy _miscAuthor ++
                     mTexy _miscTitle ++
                     mTexy _miscHowPublished ++
                     mTexy _miscMonth ++
                     mTexy _miscYear ++
                     mTexy _miscNote ++ 
                     raw "}\n"

mTexy :: (LaTeXC l, Texy a) => Maybe a -> l
mTexy = maybe "" texy

toMisc :: Identifier -> Fields -> IDocParser m b Misc
toMisc i_ (Fields f) = do
  let _miscAuthor       = mCheck AuthorF "author" f
      _miscTitle        = mCheck TitleF  "title"  f
      _miscHowPublished = mCheck HowPublishedF "howpublished" f
      _miscMonth        = mCheck MonthF "month" f
      _miscYear         = mCheck YearF "year" f
      _miscNote         = mCheck NoteF "note" f
      _miscIdentifier   = i_
  return Misc {..}

data BibEntry = ArticleBE Article
              | BookBE Book
              | MiscBE Misc
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

instance ToMarkup BibEntry where
  toMarkup (ArticleBE a_) = toMarkup a_
  toMarkup (BookBE b_) = toMarkup b_
  toMarkup (MiscBE m) = toMarkup m

instance Texy BibEntry where
  texy (ArticleBE a_) = texy a_
  texy (BookBE b_) = texy b_
  texy (MiscBE m) = texy m

data EntryType = ArticleET
               | BookET
               | BookletET
               | InBookET
               | InCollectionET
               | InProceedingsET
               | ManualET
               | MastersThesisET
               | MiscET
               | PhDThesisET
               | ProceedingsET
               | TechReportET
               | UnpublishedET
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

-- This is a handy Emacs function to do what I want.
--
-- (defun toF (str) (insert (format "\nnewtype %sF = %sF { _un%s :: Text }\n  deriving (Eq, Ord, Show, Data, Typeable, Generic)\n" str str str)))
-- (defun texify (str) (insert (format "\ninstance Texy %sF where\n  texy (%sF x) = texy $ \"%s = \" ++ x\n\n" str str (downcase str))))

newtype AddressF = AddressF { _unAddress :: Text }
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

instance Texy AddressF where
  texy (AddressF x) = raw $ "address = {" ++ x ++ "},\n"

newtype AnnoteF = AnnoteF { _unAnnote :: Text }
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

instance Texy AnnoteF where
  texy (AnnoteF x) = raw $ "annote = " ++ x

newtype AuthorF = AuthorF { _unAuthor :: Text }
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

instance Texy AuthorF where
  texy (AuthorF x) = raw $ "author = {" ++ x ++ "},\n"

newtype BookTitleF = BookTitleF { _unBookTitle :: Text }
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

instance Texy BookTitleF where
  texy (BookTitleF x) = raw $ "booktitle = {" ++ x ++ "},\n"

newtype ChapterF = ChapterF { _unChapter :: Text }
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

instance Texy ChapterF where
  texy (ChapterF x) = raw $ "chapter = {" ++ x ++ "},\n"

newtype CrossRefF = CrossRefF { _unCrossRef :: Text }
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

instance Texy CrossRefF where
  texy (CrossRefF x) = raw $ "crossref = {" ++ x ++ "},\n"

newtype EditionF = EditionF { _unEdition :: Text }
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

instance Texy EditionF where
  texy (EditionF x) = raw $ "edition = {" ++ x ++ "},\n"

newtype EditorF = EditorF { _unEditor :: Text }
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

instance Texy EditorF where
  texy (EditorF x) = raw $ "editor = {" ++ x ++ "},\n"

newtype HowPublishedF = HowPublishedF { _unHowPublished :: Text }
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

instance Texy HowPublishedF where
  texy (HowPublishedF x) = raw $ "howpublished = {" ++ x ++ "},\n"

newtype InstitutionF = InstitutionF { _unInstitution :: Text }
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

instance Texy InstitutionF where
  texy (InstitutionF x) = raw $ "instituition = {" ++ x ++ "},\n"

newtype JournalF = JournalF { _unJournal :: Text }
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

instance Texy JournalF where
  texy (JournalF x) = raw $ "journal = {" ++ x ++ "},\n"

newtype KeyF = KeyF { _unKey :: Text }
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

instance Texy KeyF where
  texy (KeyF x) = raw $ "key = {" ++ x ++ "},\n"

newtype MonthF = MonthF { _unMonth :: Text }
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

instance Texy MonthF where
  texy (MonthF x) = raw $ "monthf = {" ++ x ++ "},\n"

newtype NoteF = NoteF { _unNote :: Text }
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

instance Texy NoteF where
  texy (NoteF x) = raw $ "note = {" ++ x ++ "},\n"

newtype NumberF = NumberF { _unNumber :: Text }
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

instance Texy NumberF where
  texy (NumberF x) = raw $ "number = {" ++ x ++ "},\n"

newtype OrganizationF = OrganizationF { _unOrganization :: Text }
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

instance Texy OrganizationF where
  texy (OrganizationF x) = raw $ "organization = {" ++ x ++ "},\n"

newtype PagesF = PagesF { _unPages :: Text }
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

instance Texy PagesF where
  texy (PagesF x) = raw $ "pages = {" ++ x ++ "},\n"

newtype PublisherF = PublisherF { _unPublisher :: Text }
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

instance Texy PublisherF where
  texy (PublisherF x) = raw $ "publisher = {" ++ x ++ "},\n"

newtype SchoolF = SchoolF { _unSchool :: Text }
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

instance Texy SchoolF where
  texy (SchoolF x) = raw $ "school = {" ++ x ++ "},\n"

newtype SeriesF = SeriesF { _unSeries :: Text }
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

instance Texy SeriesF where
  texy (SeriesF x) = raw $ "series = {" ++ x ++ "},\n"

newtype TitleF = TitleF { _unTitle :: Text }
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

instance Texy TitleF where
  texy (TitleF x) = raw $ "title = {" ++ x ++ "},\n"

newtype TypeF = TypeF { _unType :: Text }
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

instance Texy TypeF where
  texy (TypeF x) = raw $ "type = {" ++ x ++ "},\n"

newtype VolumeF = VolumeF { _unVolume :: Text }
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

instance Texy VolumeF where
  texy (VolumeF x) = raw $ "volume = {" ++ x ++ "},\n"

newtype YearF = YearF { _unYear :: Text }
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

instance Texy YearF where
  texy (YearF x) = raw $ "year = {" ++ x ++ "},\n"

data FieldType = AddressFT Text
               | AnnoteFT Text
               | AuthorFT Text
               | BookTitleFT Text
               | ChapterFT Text
               | CrossRefFT Text
               | EditionFT Text
               | EditorFT Text
               | HowPublishedFT Text
               | InstitutionFT Text
               | JournalFT Text
               | KeyFT Text
               | MonthFT Text
               | NoteFT Text
               | NumberFT Text
               | OrganizationFT Text
               | PagesFT Text
               | PublisherFT Text
               | SchoolFT Text
               | SeriesFT Text
               | TitleFT Text
               | TypeFT Text
               | VolumeFT Text
               | YearFT Text
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

newtype Value = Value { _unValue :: Text }
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

newtype Identifier = Identifier { _unIdentifier :: Text }
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

newtype Fields = Fields { _unFields :: M.Map Identifier Value }
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

valuePBraced :: IDocParser m b Value
valuePBraced = do
  v <- bracedTokensP
  return $ Value $ uninterpret v

valuePQuoted :: IDocParser m b Value
valuePQuoted = do
  v <- quotedTokensP
  return $ Value $ uninterpret v

valueP :: IDocParser m b Value
valueP = do
  valE <- eitherP valuePQuoted valuePBraced
  case valE of
    Left v -> return v
    Right v -> return v

stripSpaces :: (Element seq ~ Char, IsSequence seq) => seq -> seq
stripSpaces x = reverse $ dropWhile isSpace $ reverse $ dropWhile isSpace x

-- TODO: FIXME: This is awful.  Should change to regex matching or
-- _something_.
identifierP :: IDocParser m b Identifier
identifierP = do
  t <- textP
  let t' = stripSpaces t
  if any (not . isAlphaNum) t' || null t'
    then fail $ printf "invalid identifier in bibliography: \"%s\"" t'
    else return $ Identifier t'

typeP :: IDocParser m b Identifier
typeP = do
  t <- textP
  let t' = stripSpaces t
  if any (not . isAlpha) t' || null t'
    then fail $ printf "invalid identifier in bibliography: \"%s\"" t'
    else do
    void $ tokenP Comma
    return $ Identifier t'

fieldsP :: IDocParser m b Fields
fieldsP = do
  ms <- sepBy1V (addField mempty) (tokenP Comma >> tokenP Newline)
  return $ Fields $ concat ms
  where
    addField m = do
      i_ <- identifierP
      void $ tokenP Equals
      v_ <- valueP
      return $ M.insert i_ v_ m

canonicalizeField :: Identifier -> Value -> IDocParser m b FieldType
canonicalizeField (Identifier i_) (Value v) =
  case T.toLower i_ of
    "address" -> return $ AddressFT v
    "annote"  -> return $ AnnoteFT v
    "author"  -> return $ AuthorFT v
    "booktitle" -> return $ BookTitleFT v
    "chapter" -> return $ ChapterFT v
    "crossref" -> return $ CrossRefFT v
    "edition"  -> return $ EditionFT v
    "editor"   -> return $ EditorFT v
    "howpublished" -> return $ HowPublishedFT v
    "institution" -> return $ InstitutionFT v
    "journal" -> return $ JournalFT v
    "key" -> return $ KeyFT v
    "month" -> return $ MonthFT v
    "note" -> return $ NoteFT v
    "number" -> return $ NumberFT v
    "organization" -> return $ OrganizationFT v
    "pages" -> return $ PagesFT v
    "publisher" -> return $ PublisherFT v
    "school" -> return $ SchoolFT v
    "series" -> return $ SeriesFT v
    "title" -> return $ TitleFT v
    "type" -> return $ TypeFT v
    "volume" -> return $ VolumeFT v
    "year" -> return $ YearFT v
    _ -> fail $ printf "Do not understand bibliography field of type \"%s\"" i_

entryTypeP :: IDocParser m b EntryType
entryTypeP = do
  void $ tokenP AtSign
  text_ <- textP
  case T.toLower text_ of
    "article"       -> return ArticleET
    "book"          -> return BookET
    "booklet"       -> return BookletET
    "inbook"        -> return InBookET
    "incollection"  -> return InCollectionET
    "inproceedings" -> return InProceedingsET
    "manual"        -> return ManualET
    "mastersthesis" -> return MastersThesisET
    "misc"          -> return MiscET
    "phdthesis"     -> return PhDThesisET
    "proceedings"   -> return ProceedingsET
    "techreport"    -> return TechReportET
    "unpublished"   -> return UnpublishedET
    _               -> fail $ printf "Do not understand bibliography entry of type \"%s\"" text_

bibItemP :: IDocParser m b BibItem
bibItemP = do
  et <- entryTypeP
  void $ tokenP LBrace
  rn <- typeP
  void $ tokenP Newline
  fields <- fieldsP
  void $ tokenP RBrace
  return $ BibItem et rn fields

bibTexP :: IDocParser m b BibTex
bibTexP = do
  blockStarterP
  bibItems <- someV bibItemP
  bibEntries <- sequence $ itemToEntry <$> bibItems
  tokenP Newline >> blockEnderP
  return $ BibTex bibEntries

makeLenses ''BibTex
makeLenses ''BibItem
makeLenses ''Value
makeLenses ''Identifier
makeLenses ''Fields
makeLenses ''Article
