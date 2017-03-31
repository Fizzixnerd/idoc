{-# LANGUAGE NoImplicitPrelude #-}

-- | An example module.
module Example where

import ClassyPrelude
import System.IO
import Text.IDoc.Parse
import Text.IDoc.Render.Tex
import Text.Megaparsec
import Data.Text

-- | An example function.
main :: IO ()
main = (withFile "source.idoc" ReadMode 
        (\src ->
            withFile "target.idoc" WriteMode 
            (\tgt -> do
                cnts <- System.IO.hGetContents src
                let (Right (Tex x)) = render <$> (parse (parseDoc :: ParsecT Dec String Identity Doc) "source.idoc" cnts)
                System.IO.hPutStr tgt (Data.Text.unpack x))))
