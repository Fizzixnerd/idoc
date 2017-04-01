-- | IDoc.hs
-- Author: Matt Walker
-- License: https://opensource.org/licenses/BSD-2-Clause
-- Created: Mar 29, 2017
-- Summary: Render a Doc to IDoc.

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- {-# LANGUAGE TemplateHaskell #-}
-- {-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}

module Text.IDoc.Render.IDoc where

import ClassyPrelude

class Render a where
  render :: a -> Text

