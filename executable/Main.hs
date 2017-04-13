{-# LANGUAGE NoImplicitPrelude #-}

-- It is generally a good idea to keep all your business logic in your library
-- and only use it in the executable. Doing so allows others to use what you
-- wrote in their libraries.

import qualified Example
import ClassyPrelude
import Options.Applicative

data IDoc = IDoc
  { cmd :: Command
  , outfile :: Maybe FilePath }

data Command
texOptions = undefined
htmlOptions = undefined
  
idoc :: Parser IDoc
idoc = hsubparser
  ((command "tex" (info texOptions $ progDesc "convert the file to tex.")) <>
   (command "html" (info htmlOptions $ progDesc "convert the file to html.")))

main :: IO ()
main = Example.main
