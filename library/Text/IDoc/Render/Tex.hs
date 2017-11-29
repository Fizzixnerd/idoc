{-# OPTIONS_GHC -fno-warn-orphans #-}
module Text.IDoc.Render.Tex where

import ClassyPrelude
import Control.Lens
import qualified Data.Text.IO as IO
import qualified System.IO as SIO

import Text.IDoc.Syntax as S
import Text.IDoc.Parse
import Text.IDoc.Lex

import qualified Text.Megaparsec as MP
import Text.LaTeX as L hiding ((<>))
import Text.LaTeX.Base.Class
import Text.LaTeX.Packages.Graphicx
import Text.LaTeX.Packages.AMSSymb
import Text.LaTeX.Packages.AMSMath as M
import Text.LaTeX.Packages.AMSThm as T
import Text.LaTeX.Packages.Hyperref

type Icon = LaTeX

mLabel :: (LaTeXC l) => Maybe SetID -> (l -> l)
mLabel mid = case mid of
  Nothing -> id
  Just id_ -> ((texy id_) ++)

defaultDecorator :: LaTeXC l => l -> l -> l
defaultDecorator title_ d =
  (documentclass [letterpaper] "memoir") ++

  (usepackage [] hyperref) ++
  (usepackage [] graphicx) ++
  (usepackage [] amsmath) ++
  (usepackage [] amsthm) ++
  (usepackage [] amssymb) ++

  (document $
  title title_ ++
  author "ILS Contributors" ++
  date today ++
  maketitle ++
  tableofcontents ++
  d)

compileToTex :: FilePath -> FilePath -> IO ()
compileToTex inFile outFile = SIO.withFile inFile SIO.ReadMode 
                              (\src -> do
                                  fileContents <- IO.hGetContents src
                                  case MP.parse dTokens inFile fileContents of
                                    Left e -> print e
                                    Right x -> do
                                      case MP.parse docP "<tokens>" x of
                                        Left e -> print e
                                        Right y -> renderFile outFile $ defaultDecorator "Title" $ (texy y :: LaTeX))
                                          

instance Texy Doc where
  texy d = chapter (mLabel (d^.docSetID) $ (texy $ d^.docTitle)) ++
           (vectorBlockTexy $ d^.docSections)

instance Texy Section where
  texy s = starter (mLabel (s^.secSetID) $ texy $ s^.secTitle) ++
           (vectorBlockTexy $ s^.secContents)
    where starter = case s^.secType of
            Preamble -> const ""
            TopSection -> section
            SubSection -> subsection

instance Texy SectionTitle where
  texy (SectionTitle s) = vectorBlockTexy s

instance Texy DocTitle where
  texy (DocTitle dt) = vectorBlockTexy dt

instance Texy Core where
  texy (SC sc) = texy sc
  texy (CC cc) = texy cc

instance Texy SimpleCore where
  texy (TextC t) = texy t
  texy (QTextC qt) = texy qt
  texy (LinkC l) = texy l
  texy (InlineMathC im) = texy im
  texy (MarkupC m) = texy m

instance Texy ComplexCore where
  texy (ListC l) = texy l
  texy (BlockC b) = texy b
  texy (ParagraphC p) = texy p

instance Texy QText where
  texy qt = decorateTextWith (qt^.qtType) $
            concatMap texy $ qt^.qtText
    where
      decorateTextWith Strong = textbf
      decorateTextWith Emphasis = emph
      decorateTextWith Monospace = texttt
      decorateTextWith Superscript = textsuperscript
      decorateTextWith Subscript = textsubscript
      decorateTextWith Quoted = qts

      textsuperscript x = between x (raw "\textsuperscript{") (raw "}")

      textsubscript x = between x (raw "\textsubscript{") (raw "}")

idHelper :: (Text -> t) -> ID -> t
idHelper decorator id_ = let (base, hash_) =
                               case (id_^.idProtocol, id_^.idHash) of
                                 (Just (Protocol "youtube"), Nothing) -> 
                                   ("https://youtube.com/embed/", "")
                                 (Just (Protocol "youtube"), Just _) -> 
                                   error "got youtube protocol with a hash!?"
                                 (Nothing, Nothing) -> 
                                   ("http://www.independentlearning.science/tiki/", "")
                                 (Just (Protocol p), Just (IDHash h)) ->
                                   (p ++ "://", h)
                                 (Nothing, Just (IDHash h)) -> ("/", h)
                                 (Just (Protocol p), Nothing) -> (p ++ "://", "")
                         in
                           decorator $ base ++
                           (concatMap (\(IDBase x) -> x) $ intersperse (IDBase "/") (id_^.idBase)) ++
                           hash_

instance Texy ID where
  texy id_ = idHelper texy id_

instance Texy SetID where
  texy (SetID { _sidName = IDHash sid }) = label $ texy sid

instance Texy Link where
  texy l = ref $ texy $ l^.linkText

instance Texy LinkText where
  texy (LinkText lt) = concatMap texy lt

instance Texy InlineMath where
  texy im = M.math $ concatMap texy $ im^.imContents

instance Texy Token where
  texy t = raw $ unToken t

instance Texy Markup where
  texy mu = mLabel (mu^.muSetID) $
    case (mu^.muType) of
      Footnote -> footnote $ concatMap texy $ mu^.muContents
      FootnoteRef -> ref $ concatMap texy $ mu^.muContents
      Citation -> cite $ concatMap texy $ mu^.muContents

instance Texy Paragraph where
  texy p = (concatMap texy $ p^.paraContents) ++ "\n\n"

vectorBlockTexy :: (Texy a, LaTeXC l) => Vector a -> l
vectorBlockTexy vb = concatMap texy vb

instance Texy BlockTitle where
  texy (BlockTitle bt) = concatMap texy bt

instance Texy Block where
  texy b = case b^.bType of
             PrerexB p -> block (b^.bTitle) (b^.bSetID) p
             IntroductionB i -> block (b^.bTitle) (b^.bSetID) i
             MathB m -> block (b^.bTitle) (b^.bSetID) m
             EquationB e -> block (b^.bTitle) (b^.bSetID) e
             EqnArrayB e -> block (b^.bTitle) (b^.bSetID) e
             TheoremB t -> block (b^.bTitle) (b^.bSetID) t
             LemmaB l -> block (b^.bTitle) (b^.bSetID) l
             CorollaryB c -> block (b^.bTitle) (b^.bSetID) c
             PropositionB p -> block (b^.bTitle) (b^.bSetID) p
             ConjectureB c -> block (b^.bTitle) (b^.bSetID) c
             AxiomB a -> block (b^.bTitle) (b^.bSetID) a
             ProofB p -> block (b^.bTitle) (b^.bSetID) p
             QuoteB q -> block (b^.bTitle) (b^.bSetID) q
             CodeB  c -> block (b^.bTitle) (b^.bSetID) c
             ImageB i -> block (b^.bTitle) (b^.bSetID) i
             VideoB v -> block (b^.bTitle) (b^.bSetID) v
             ConnectionB c -> block (b^.bTitle) (b^.bSetID) c
             DefinitionB d -> block (b^.bTitle) (b^.bSetID) d
             IntuitionB i -> block (b^.bTitle) (b^.bSetID) i
             YouTubeB y -> block (b^.bTitle) (b^.bSetID) y
             InfoB i -> block (b^.bTitle) (b^.bSetID) i
             TipB t -> block (b^.bTitle) (b^.bSetID) t
             CautionB c -> block (b^.bTitle) (b^.bSetID) c
             WarningB w -> block (b^.bTitle) (b^.bSetID) w
             SideNoteB s -> block (b^.bTitle) (b^.bSetID) s
             ExampleB e -> block (b^.bTitle) (b^.bSetID) e
             ExerciseB e -> block (b^.bTitle) (b^.bSetID) e
             BibliographyB b_ -> block (b^.bTitle) (b^.bSetID) b_
             FurtherReadingB f -> block (b^.bTitle) (b^.bSetID) f
             SummaryB s -> block (b^.bTitle) (b^.bSetID) s
             RecallB r -> block (b^.bTitle) (b^.bSetID) r
      
mTitle :: LaTeXC l => Maybe BlockTitle -> Text -> l
mTitle mbt defaultTitle = maybe (texy defaultTitle) texy mbt

class Blocky b where
  block :: LaTeXC l => Maybe BlockTitle -> Maybe SetID -> b -> l

instance Blocky Prerex where
  block mt msid (Prerex ps) = subsubsection (mLabel msid title) ++ vectorBlockTexy ps
    where
      title = mTitle mt "Prerex"

instance Texy PrerexItem where
  texy p = (href [] "" $ texy $ p^.prerexItemPath) ++
           ": " ++
           (concatMap texy $ p^.prerexItemDescription)

instance Blocky Introduction where
  block mt msid (Introduction i) = subsection (mLabel msid title) ++ vectorBlockTexy i
    where
      title = mTitle mt "Introduction"

instance Blocky Math where
  block mt msid (Math m) = mLabel msid $ mathDisplay $ raw $ concatMap unToken m

instance Blocky Equation where
  block mt msid (Equation e) = mLabel msid $ M.equation $ raw $ concatMap unToken e

instance Blocky EqnArray where
  block mt msid (EqnArray e) = mLabel msid $ M.align [raw $ concatMap unToken e]

theoremBlock :: LaTeXC l => 
                Maybe BlockTitle
             -> Maybe SetID
             -> Vector Core
             -> Maybe (Vector Core)
             -> String
             -> l
theoremBlock mt msid thm mprf ttype = mLabel msid $
                                      T.theorem ttype $
                                      vectorBlockTexy thm ++
                                      maybe "" (T.proof (texy <$> mt) . vectorBlockTexy) mprf

instance Blocky Theorem where
  block mt msid (Theorem (thm, mprf)) = theoremBlock mt msid thm mprf "Theorem"

instance Blocky Lemma where
  block mt msid (Lemma (thm, mprf)) = theoremBlock mt msid thm mprf "Lemma"

instance Blocky Corollary where
  block mt msid (Corollary (thm, mprf)) = theoremBlock mt msid thm mprf "Corollary"

instance Blocky Proposition where
  block mt msid (Proposition (thm, mprf)) = theoremBlock mt msid thm mprf "Proposition"

instance Blocky Conjecture where
  block mt msid (Conjecture c) = mLabel msid $
                                 T.theorem "Conjecture" $
                                 vectorBlockTexy c

instance Blocky Axiom where
  block mt msid (Axiom a) = mLabel msid $
                            T.theorem "Axiom" $
                            vectorBlockTexy a

instance Blocky Proof where
  block mt msid (Proof p) = mLabel msid $
                            T.proof (texy <$> mt) $
                            vectorBlockTexy p

instance Blocky Quote where
  block mt msid (Quote q) = mLabel msid $
                            L.quote $
                            vectorBlockTexy q

instance Blocky Code where
  block mt msid (Code c) = mLabel msid $
                           L.verb $ concatMap unToken c

instance Blocky Image where
  block mt msid (Image (lnk, mcaption)) = mLabel msid $
                                          L.verb "THIS ISN'T IMPLEMENTED (Image): " ++
                                          maybe "" vectorBlockTexy mcaption

instance Blocky Video where
  block mt msid (Video (ln, mcaption)) = mLabel msid $ 
                                         L.verb "THIS ISN'T IMPLEMENTED (Video): " ++
                                         maybe "" vectorBlockTexy mcaption

instance Blocky Connection where
  block mt msid (Connection c) = (subsection $ mLabel msid title) ++
                                 vectorBlockTexy c
    where title = mTitle mt "Connection"

instance Blocky Definition where
  block mt msid (S.Definition d) = mLabel msid $
                                   T.theorem "Definition" $
                                   vectorBlockTexy d

instance Blocky Intuition where
  block mt msid (Intuition i) = (subsubsection $ mLabel msid title) ++
                                vectorBlockTexy i
    where title = mTitle mt "Intuition"

instance Blocky YouTube where
  block mt msid (YouTube (lnk, mcaption)) = mLabel msid $ 
                                            L.verb "THIS ISN'T IMPLEMENTED (YouTube): " ++
                                            maybe "" vectorBlockTexy mcaption

instance Blocky Info where
  block mt msid (Info i) = (subsubsection $ mLabel msid title) ++
                           vectorBlockTexy i
    where
      title = mTitle mt "Info"

instance Blocky Tip where
  block mt msid (Tip t) = (subsubsection $ mLabel msid title) ++
                          vectorBlockTexy t
    where
      title = mTitle mt "Tip"

instance Blocky Caution where
  block mt msid (Caution c) = (subsubsection $ mLabel msid title) ++
                              vectorBlockTexy c
    where
      title = mTitle mt "Caution"

instance Blocky Warning  where
  block mt msid (Warning w) = (subsubsection $ mLabel msid title) ++
                              vectorBlockTexy w
    where 
      title = mTitle mt "Warning"

instance Blocky SideNote where
  block mt msid (SideNote s) = (subsubsection $ mLabel msid title) ++
                               vectorBlockTexy s
    where
      title = mTitle mt "SideNote"

instance Blocky Example where
  block mt msid (Example (ex, ans)) = (subsubsection $ mLabel msid title) ++
                                      vectorBlockTexy ex ++
                                      vectorBlockTexy ans
    where
      title = mTitle mt "Example"

instance Blocky Exercise where
  block mt msid (Exercise e) = (subsubsection $ mLabel msid title) ++
                               vectorBlockTexy e
    where
      title = mTitle mt "Exercise"

instance Blocky Bibliography where
  block mt msid (Bibliography b) = (subsection $ mLabel msid title) ++
                                   vectorBlockTexy b
    where
      title = mTitle mt "Bibliography"

instance Texy BibItem where
  texy bi = texy (bi^.biTitle) ++
            ": " ++
            texy (bi^.biAuthor) ++
            "; " ++
            texy (bi^.biYear) ++
            "."

instance Blocky FurtherReading where
  block mt msid (FurtherReading f) = (subsection $ mLabel msid title) ++
                                      vectorBlockTexy f
    where
      title = mTitle mt "Further Reading"

instance Blocky Summary where
  block mt msid (Summary s) = (subsection $ mLabel msid title) ++
                              vectorBlockTexy s
    where
      title = mTitle mt "Summary"

instance Blocky Recall where
  block mt msid (Recall (lnks, r)) = (subsubsection $ mLabel msid title) ++
                                     vectorBlockTexy r
    where
      title = mTitle mt "Recall"

instance Texy S.List where
  texy (S.List li) = enumerate $
                     concatMap (\li_ -> 
                                  mLabel (li_^.liSetID) $ 
                                  item (texy <$> (li_^.liLabel)) ++ (vectorBlockTexy $ li_^.liContents)) li


instance Texy ListLabel where
  texy (ListLabel ll) = vectorBlockTexy ll


-- instance Blocky Equation where
--   block mt msid = 

-- block :: LaTeXC l =>
--          l -- ^ title
--       -> Maybe SetID -- ^ id
--       -> l -- ^ icon
--       -> Maybe l -- ^ footer
--       -> l -- ^ body
--       -> l
-- block title_ id_ icon__ footer_ body_ =
--   mfooterify footer_ $
--   (subsubsection $ mLabel id_ $ icon__ L.<> " " L.<> title_) L.<>
--   body_
--   where
--     mfooterify Nothing x = x
--     mfooterify (Just f) x = (f L.<> newline L.<> x)



