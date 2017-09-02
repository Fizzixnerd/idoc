-- | Icons.hs
-- Author: Matt Walker
-- License: https://opensource.org/licenses/BSD-2-Clause
-- Created: Aug 31, 2017
-- Summary: 

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Text.IDoc.Render.Html5.Icons where

import ClassyPrelude

import qualified Text.Blaze.Html5 as B
import qualified Text.Blaze.Html5.Attributes as A

type Icon = B.Html

icon :: B.AttributeValue -> Icon
icon x = B.span B.! A.class_ ("fa " ++ x) $ ""

prerexItemIcon :: Icon
prerexItemIcon = icon "fa-question-circle"

youTubeIcon :: Icon
youTubeIcon = icon "fa-youtube"

infoIcon :: Icon
infoIcon = icon "fa-info-circle"

tipIcon :: Icon
tipIcon = icon "fa-lightbulb-o"

cautionIcon :: Icon
cautionIcon = icon "fa-exclamation-triangle"

warningIcon :: Icon
warningIcon = icon "fa-exclamation-circle"
