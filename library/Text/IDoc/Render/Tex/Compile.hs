module Text.IDoc.Render.Tex.Compile where

import Text.IDoc.Parse

import qualified Data.Text.IO as IO
import qualified System.IO as SIO

import qualified Text.Megaparsec as MP

import ClassyPrelude

compileToTex :: LaTeX -> FilePath -> FilePath -> IO ()
compileToTex title_ inFile outFile = SIO.withFile inFile SIO.ReadMode 
                                     (\src -> do
                                         fileContents <- IO.hGetContents src
                                         case MP.parse dTokens inFile fileContents of
                                           Left e -> print e
                                           Right x -> do
                                             case MP.parse docP "<tokens>" x of
                                               Left e -> print e
                                               Right y -> renderFile outFile $ defaultDecorator title_ $ (texy y :: LaTeX))
