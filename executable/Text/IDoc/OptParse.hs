{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}

module Text.IDoc.OptParse where

import Control.Lens

import ClassyPrelude
import Options.Applicative as OP

import qualified Data.Text.IO as TIO
import qualified System.IO as IO

import qualified Text.IDoc.Syntax as S
import Text.IDoc.Lang.Ils
import Text.IDoc.Render.Tex
import Text.Megaparsec.Error

import qualified Text.Blaze.Html5 as B
import qualified Text.Blaze.Html.Renderer.Text as R
import qualified Text.LaTeX as L

data Program = Program { _source :: FilePath
                       , _target :: FilePath
                       , _action_ :: Action
                       }

data Action = Parse | Html | Tex | CheckLinks

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
                             case compileIls' False cnts of
                               Left e -> IO.hPutStr outh (errorBundlePretty e)
                               Right x -> case x of
                                 Left e -> IO.hPutStr outh (errorBundlePretty e)
                                 Right y -> m y outh)
      action' Parse = doIt (\y outh -> TIO.hPutStr outh (tshow y))
      action' Html  = doIt (\y outh -> TIO.hPutStr outh (pack $ unpack $ R.renderHtml $ B.toMarkup y))
      action' Tex   = doIt (\y outh -> TIO.hPutStr outh (L.render $ defaultDecorator (concatMap L.texy $ y^.S.docTitle.S.unDocTitle) $ (L.texy y :: L.LaTeX)))
      action' CheckLinks = doIt (\y outh -> TIO.hPutStr outh (tshow $ (\(S.BadLink link_ _) -> link_) <$> (S.checkLinks (S.Constraints mempty mempty) (Nothing :: Maybe (S.Core MarkupType BlockType)) y)))
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
    <*> subparser (parse <> html <> tex <> checklinks)


parse :: Mod CommandFields Action
parse = command "parse" (info (pure Parse) $
                         progDesc "Parse the input and dump the parse tree to the output.")


html :: Mod CommandFields Action
html = command "html" (info (pure Html) $
                       progDesc "Output the HTML representation of the input.")

tex :: Mod CommandFields Action
tex = command "tex" (info (pure Tex) $
                     progDesc "Output the TeX representation of the input.")

checklinks :: Mod CommandFields Action
checklinks = command "checklinks" (info (pure CheckLinks) $
                                   progDesc "Check the links and output the bad ones.")
