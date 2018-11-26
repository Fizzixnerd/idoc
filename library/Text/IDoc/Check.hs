-- | Check.hs
-- Author: Matt Walker
-- License: https://opensource.org/licenses/BSD-2-Clause
-- Created: Sep 02, 2017
-- Summary:

module Text.IDoc.Check where

import ClassyPrelude as CP

-- import qualified Data.Set as Set
-- -- import Text.Printf
-- import Control.Monad.State
-- import Control.Monad.Except
import qualified Data.Vector as V

import Control.Lens
import Data.Vinyl.CoRec

-- import Text.IDoc.Blocks.Admonition
-- import Text.IDoc.Blocks.BibTex
-- import Text.IDoc.Blocks.Code
-- import Text.IDoc.Blocks.Connection
-- import Text.IDoc.Blocks.Example
-- import Text.IDoc.Blocks.Exercise
-- import Text.IDoc.Blocks.FurtherReading
-- import Text.IDoc.Blocks.IntroOutro
-- import Text.IDoc.Blocks.Intuition
-- import Text.IDoc.Blocks.Math
-- import Text.IDoc.Blocks.Media
-- import Text.IDoc.Blocks.Prerex
-- import Text.IDoc.Blocks.Quote
-- import Text.IDoc.Blocks.Recall
-- import Text.IDoc.Markup.Footnote
-- import Text.IDoc.Markup.FootnoteRef
-- import Text.IDoc.Markup.Citation

import qualified Text.IDoc.Syntax as S
import qualified Text.IDoc.Lang.Ils as S
import qualified Text.IDoc.Blocks.Prerex as S

-- | So in ILS-flavored idoc we need to check a bunch of things to make sure the
-- input is well formed. First of all, we have to check that every link is legal
-- according to the rules of idoc, where you can only have blinks and ilinks
-- except in specific places like FurtherReadings and the Bibliography. We also
-- need to ensure blinks actually point to things in the prerequisites.
--
-- We also need to ensure that there is a Prerex block as the first thing in the
-- Doc, as well as the first thing in a Connection. We must ensure that the
-- Bibliography comes at the end of the Doc.
--
-- There should also be warnings, for things like not having a Bibliography,
-- Introduction, or Summary.
--
-- The checker could maybe also merge nearby TextCs, though this probably should
-- be done in a separate pass to be honest.

-- newtype Warning = Warning { unWarning :: Text }
--   deriving (Eq, Ord, Show)

-- data CheckState = CheckState { warnings :: Vector Warning }
--   deriving (Eq, Ord, Show)

data CheckError = NoSectionsError
                | NoPreambleError
--                | EmptyPreambleError
--                | EmptyPrerexError S.Prerex
                | NoStartingPrerexError
--                | EmptyConnectionError S.Connection
--                | NoConnectionPrerexError S.Connection
--                | OLinkNotAllowed S.Link
--                | LinkNotAllowed S.Link
  deriving (Eq, Ord, Show)

-- newtype Check a = Check { unCheck :: (ExceptT CheckError
--                                       (StateT CheckState Identity)) a }
--   deriving (Functor, Applicative, Monad, MonadState CheckState,
--             MonadError CheckError)

-- emptyCheckState :: CheckState
-- emptyCheckState = CheckState { warnings = empty }

-- runCheck :: Check a -> Either CheckError a
-- runCheck c = fst $
--              runIdentity $
--              runStateT (runExceptT $ unCheck c) emptyCheckState

-- -- | Assumes nothing.
-- checkBasicWellFormedness :: S.IlsDoc -> Check S.IlsDoc
-- checkBasicWellFormedness d@(S.Doc { S._docSections = s }) = do
--   when (null s) $
--     throwError NoSectionsError
--   let preamble = V.head s
--   when (preamble^.S.secType /= S.Preamble) $
--     throwError NoPreambleError
--   -- when (null $ preamble^.S.secContents) $
--   --   throwError EmptyPreambleError
--   return d

fetchPrerex :: S.IlsDoc -> Maybe (S.Prerex S.MarkupType)
fetchPrerex (S.Doc {S._docSections = s}) = do
  preamble <- if null s then Nothing else Just $ V.head s
  prerexCandidate <- if null $ preamble^.S.secContents then Nothing else Just $  preamble^.S.secContents.to V.head
  case prerexCandidate of
    S.CC (S.BlockC (S.Block {S._bType = (S.BlockType bty) })) -> asA bty
    _ -> Nothing

-- -- | False indicates the link does not fall within the constraints.
-- checkLinkAgainstConstraints :: Constraints -> S.Link m -> Bool
-- checkLinkAgainstConstraints constraints (S.Link { S._linkType = S.Internal
--                                                 , S._linkLocation = location }) =
--   let S.ID { S._idBase = idBase } = location
--   in
--     PrerexConstraint idBase `Set.member` constraints
-- checkLinkAgainstConstraints _ _ = True

-- checkLinks :: Set PrerexConstraint -> S.Core m b -> Vector (BadLink m b)
-- checkLinks constraints c@(S.SC sc) = checkSimpleCoreLinks constraints c sc
-- checkLinks constraints c@(S.CC (S.ListC (S.List listItems))) =
--   let contents = concatMap (\li -> li^.S.liContents) listItems
--   in
--     concatMap (checkSimpleCoreLinks constraints c) contents
-- checkLinks constraints c@(S.CC (S.BlockC block)) = checkBlockLinks constraints c block

-- checkSimpleCoreLinks :: Set PrerexConstraint -> S.Core m b -> S.SimpleCore m -> Vector (BadLink m b)
-- checkSimpleCoreLinks constraints container (S.LinkC l) = if checkLinkAgainstConstraints constraints l
--                                                          then empty
--                                                          else V.singleton BadLink
--                                                               { _blLink = l
--                                                               , _blLocation = container
--                                                               }
-- checkSimpleCoreLinks _ _ _ = empty

-- checkBlockLinks :: Set PrerexConstraint
--                 -> S.Core S.MarkupType S.BlockType
--                 -> S.Block S.MarkupType S.BlockType
--                 -> Vector (BadLink S.MarkupType S.BlockType)
-- checkBlockLinks constraints container block =
--   let blockTitleSimpleCores = maybe empty S.unBlockTitle (block^.S.bTitle)
--       blockTitleBadLinks = concatMap (checkSimpleCoreLinks constraints container) blockTitleSimpleCores
--       blockTypeBadLinks = match (block^.S.bType) $
--         (H checkAdmonitionLinks)
--         :& (H checkBibTexLinks)
--         :& (H checkCodeLinks)
--         :& (H checkConnectionLinks)
--         :& (H checkExampleLinks)
--         :& (H checkExerciseLinks)
--         :& (H checkFurtherReadingLinks)
--         :& (H checkIntroductionLinks)
--         :& (H checkSummaryLinks)
--         :& (H checkIntuitionLinks)
--         :& (H checkDisplayMathLinks)
--         :& (H checkTheoremLikeLinks)
--         :& (H checkConjectureLinks)
--         :& (H checkDefinitionLinks)
--         :& (H checkProofLinks)
--   in
--     blockTitleBadLinks ++ blockTypeBadLinks

-- -- | Assumes basic well-formedness.
-- checkForPrerex :: S.Doc m b -> Check S.Doc
-- checkForPrerex d@(S.Doc { S._docSections = s }) = do
--   let preamble = V.head s
--       mDocPrerex = preamble^.S.secContents.to V.headM :: Maybe (S.Core m b)
--   case docPrerex of
--     (S.CC (S.BlockC (S.Block { S._bType = S.PrerexB (S.Prerex p) }))) -> do
--       when (null p) $
--         throwError $ EmptyPrerexError (S.Prerex p)
--     _ -> throwError $ NoStartingPrerexError
--   CP.mapM_ checkConnectionForPrerex $ allConnections d
--   return d
--   where
--     isConnection (S.CC (S.BlockC (S.Block
--                                   { S._bType = S.ConnectionB _ }))) = True
--     isConnection _ = False

--     findConnectionsInSection s_ = filter isConnection $ s_^.S.secContents

--     allConnections d_ = concat $ findConnectionsInSection <$> (d_^.S.docSections)

--     checkConnectionForPrerex (S.CC (S.BlockC
--                                     (S.Block
--                                       { S._bType = S.ConnectionB c_ }))) = do
--       when (null $ c_^.S.connectionContents) $
--         throwError $ EmptyConnectionError c_
--       let connectionPrerex = c_^.S.connectionContents.to V.head
--       case connectionPrerex of
--         (S.CC (S.BlockC (S.Block { S._bType = S.PrerexB (S.Prerex p) }))) -> do
--           when (null p) $
--             throwError $ EmptyPrerexError (S.Prerex p)
--         _ -> throwError $ NoConnectionPrerexError c_
--     checkConnectionForPrerex _ = error "You called this function on a non-connection."

-- -- | Assumes basic well-formedness and good prerex.
-- checkLinks :: S.Doc -> Check S.Doc
-- checkLinks d@(S.Doc { S._docSections = s }) = do
--   undefined
--   where
--     checkCores :: Set PrerexConstraint -> Bool -> Vector S.Core -> Check ()
--     checkCores ps allowOlinks cs = CP.mapM_ (checkCore ps allowOlinks) cs

--     checkCore :: Set PrerexConstraint -> Bool -> S.Core -> Check ()
--     checkCore ps allowOlinks (S.CC (S.BlockC b)) = checkBlock ps allowOlinks b
--     checkCore ps allowOlinks (S.CC (S.ListC l)) = checkList ps allowOlinks l
--     checkCore ps allowOlinks (S.CC (S.ParagraphC p)) =
--       checkParagraph ps allowOlinks p

--     checkPrerexItemForLinks (S.PrerexItem { S._prerexItemDescription = pid }) =
--       CP.mapM_ checkSimpleCoreForLinks pid

--     checkSimpleCoreForLinks (S.LinkC l) = throwError $ LinkNotAllowed l
--     checkSimpleCoreForLinks _ = return ()

--     checkBlock :: Set PrerexConstraint -> Bool -> S.Block -> Check ()
--     checkBlock ps allowOlinks b = checkBlockType ps allowOlinks $ b^.S.bType

--     checkBlockType :: Set PrerexConstraint -> Bool -> S.BlockType -> Check ()
--     checkBlockType ps allowOlinks bt =
--       case bt of
--         S.PrerexB (S.Prerex pitems) -> CP.mapM_ checkPrerexItemForLinks pitems
--         S.IntroductionB (S.Introduction i) -> checkCores ps allowOlinks i
--         S.MathB _ -> return ()
--         S.EquationB _ -> return ()
--         S.EqnArrayB _ -> return ()
--         S.TheoremB (S.Theorem (xs, mys)) -> do
--           checkCores ps allowOlinks xs
--           CP.mapM_ (checkCores ps allowOlinks) mys
--         S.LemmaB (S.Lemma (xs, mys)) -> do
--           checkCores ps allowOlinks xs
--           CP.mapM_ (checkCores ps allowOlinks) mys
--         S.CorollaryB (S.Corollary (xs, mys)) -> do
--           checkCores ps allowOlinks xs
--           CP.mapM_ (checkCores ps allowOlinks) mys
--         S.PropositionB (S.Proposition (xs, mys)) -> do
--           checkCores ps allowOlinks xs
--           CP.mapM_ (checkCores ps allowOlinks) mys
--         S.ConjectureB (S.Conjecture c) -> checkCores ps allowOlinks c
--         S.AxiomB (S.Axiom a) -> checkCores ps allowOlinks a
--         S.ProofB (S.Proof p) -> checkCores ps allowOlinks p
--         S.QuoteB (S.Quote q) -> checkSimpleCores ps allowOlinks q
--         S.CodeB _ -> return ()
--         S.ImageB (S.Image (_, mys)) ->
--           CP.mapM_ (checkSimpleCores ps allowOlinks) mys
--         S.VideoB (S.Video (_, mys)) ->
--           CP.mapM_ (checkSimpleCores ps allowOlinks) mys
--         S.YouTubeB (S.YouTube (_, mys)) ->
--           CP.mapM_ (checkSimpleCores ps allowOlinks) mys
--         S.ConnectionB (S.Connection c) -> do
--           -- The big one.
--           let newPs = gatherPrerexConstraints $ c^.to V.head
--           checkCores (ps `union` newPs) allowOlinks c
--         S.DefinitionB (S.Definition d) -> checkCores ps allowOlinks d
--         S.IntuitionB (S.Intuition i) -> checkCores ps allowOlinks i
--         S.InfoB (S.Info i) -> checkCores ps allowOlinks i
--         S.TipB (S.Tip t) -> checkCores ps allowOlinks t
--         S.CautionB (S.Caution c) -> checkCores ps allowOlinks c
--         S.WarningB (S.Warning w) -> checkCores ps allowOlinks w
--         S.SideNoteB (S.SideNote s) -> checkCores ps allowOlinks s
--         S.ExampleB (S.Example (xs, ys)) -> do
--           checkCores ps allowOlinks xs
--           checkCores ps allowOlinks ys
--         S.ExerciseB (S.Exercise e) -> checkCores ps allowOlinks e
--         S.BibliographyB (S.Bibliography b) -> return ()
--         S.FurtherReadingB (S.FurtherReading f) -> do
--           -- Another important one
--           checkCores ps True f
--         S.SummaryB (S.Summary s) -> checkCores ps allowOlinks s
--         S.RecallB (S.Recall (ls, xs)) -> do
--           CP.mapM_ (checkLink ps allowOlinks) ls
--           checkCores ps allowOlinks xs

--     checkList ps allowOlinks (S.List { S._listContents = l }) =
--       CP.mapM_ (checkListItem ps allowOlinks) l

--     checkListItem ps allowOlinks (S.ListItem { S._liContents = li }) =
--       checkSimpleCores ps allowOlinks li

--     checkParagraph ps allowOlinks (S.Paragraph
--                                    { S._paraContents = p }) =
--       checkSimpleCores ps allowOlinks p

--     checkSimpleCores :: Set PrerexConstraint
--                      -> Bool
--                      -> Vector S.SimpleCore
--                      -> Check ()
--     checkSimpleCores ps allowOlinks ls =
--       CP.mapM_ (checkSimpleCore ps allowOlinks) ls

--     checkSimpleCore ps allowOlinks (S.LinkC l) = checkLink ps allowOlinks l
--     checkSimpleCore ps allowOlinks _ = return ()

--     checkLink :: Set PrerexConstraint -> Bool -> S.Link -> Check ()
--     checkLink ps allowOlinks l = case l^.S.linkType of
--       S.Internal -> return ()
--       S.Back -> undefined
--       S.Out -> do
--         unless allowOlinks $
--           throwError $ OLinkNotAllowed l
--         return ()

