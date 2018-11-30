-- | Check.hs
-- Author: Matt Walker
-- License: https://opensource.org/licenses/BSD-2-Clause
-- Created: Sep 02, 2017
-- Summary:

module Text.IDoc.Check where

import ClassyPrelude as CP

import qualified Data.Vector as V

import Control.Lens
import Data.Vinyl.CoRec

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

fetchPrerex :: S.IlsDoc -> Maybe (S.Prerex S.MarkupType)
fetchPrerex (S.Doc {S._docSections = s}) = do
  preamble <- headMay s
  prerexCandidate <- headMay $ preamble^.S.secContents
  case prerexCandidate of
    S.CC (S.BlockC (S.Block {S._bType = (S.BlockType bty) })) -> asA bty
    _ -> Nothing
