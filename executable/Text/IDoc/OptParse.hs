{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}

module Text.IDoc.OptParse where

import Control.Lens

import ClassyPrelude
import Options.Applicative as OP

import qualified Data.Text.IO as TIO
import qualified System.IO as IO

import Text.IDoc.Parse
import Text.IDoc.Lex
import qualified Text.IDoc.Syntax as S
import Text.IDoc.Lang.Ils
import Text.IDoc.Render.Tex

import qualified Text.Megaparsec as MP
import qualified Text.Blaze.Html5 as B
import qualified Text.Blaze.Html.Renderer.Text as R
import qualified Text.LaTeX as L

data Program = Program { _source :: FilePath
                       , _target :: FilePath
                       , _action_ :: Action
                       }

data Action = Parse | Html | Tex

makeLenses ''Program

program :: Program -> IO ()
program p =
  let withInFile f = if null $ p^.source then f stdin else IO.withFile (p^.source) IO.ReadMode f
      withOutFile f = if null $ p^.target then f stdout else IO.withFile (p^.target) IO.WriteMode f
      withFiles m = withInFile
                    (\inh ->
                        withOutFile
                        (\outh -> m inh outh))
      doIt m = withFiles (\inh outh -> do
                             cnts <- TIO.hGetContents inh
                             case MP.parse dTokens "<file>" cnts of
                               Left e -> print e
                               Right x -> case MP.parse (docP blockTypeP) "<tokens>" x of
                                 Left e -> print e
                                 Right y -> m y outh)
      action' Parse = doIt (\y outh -> TIO.hPutStr outh (fromString $ show y))
      action' Html  = doIt (\y outh -> TIO.hPutStr outh (pack $ unpack $ R.renderHtml $ B.toMarkup y))
      action' Tex   = doIt (\y outh -> TIO.hPutStr outh (L.render $ defaultDecorator (concatMap L.texy $ S.unDocTitle $ y^.S.docTitle) $ (L.texy y :: L.LaTeX)))
  in 
    action' $ p^.action_

idoc :: Parser Program
idoc =  Program
    <$> strOption
        ( long "source"
       <> short 's'
       <> metavar "INFILE"
       <> value ""
       <> help "Input file to process.  Will read from stdin if omitted." )
    <*> strOption
        ( long "target"
       <> short 't'
       <> metavar "OUTFILE"
       <> value ""
       <> help "File to which to write the output.  Will write to stdout if omitted." )
    <*> subparser (parse <> html <> tex)

parse = command "parse" (info (pure Parse) $
                          (progDesc "Parse the input and dump the parse tree to the output."))


html = command "html" (info (pure Html) $
                        progDesc "Output the HTML representation of the input.")

tex = command "tex" (info (pure Tex) $
                      progDesc "Output the TeX representation of the input.")

