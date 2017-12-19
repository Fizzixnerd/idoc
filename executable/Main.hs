{-# LANGUAGE NoImplicitPrelude #-}

-- It is generally a good idea to keep all your business logic in your library
-- and only use it in the executable. Doing so allows others to use what you
-- wrote in their libraries.

import ClassyPrelude
import Text.IDoc.OptParse
import Options.Applicative

main :: IO ()
main = do
  p <- execParser idoc'
  program p
  where 
    idoc' = info (idoc <**> helper)
            (fullDesc
             <> progDesc "Compile idoc files to HTML5 or TeX."
             <> header "idoc -- documents for the Prerex engine.")
