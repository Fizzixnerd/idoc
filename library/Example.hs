{-# LANGUAGE NoImplicitPrelude #-}

-- | An example module.
module Example where

import ClassyPrelude
import System.IO
import Text.IDoc.Parse
-- import Text.IDoc.Render.Tex
import Text.IDoc.Render.Hamlet
import Text.Megaparsec



-- main :: IO ()
-- main = (withFile "source.idoc" ReadMode
--          (\src -> do
--              cnts <- System.IO.hGetContents src
--              System.IO.print $ fmap GP.doc $ parse (ils :: ParsecT Dec String Identity (Doc Main)) "source.idoc" cnts))


-- -- | An example function.
-- main' :: IO ()
-- main' = (withFile "source.idoc" ReadMode 
--          (\src ->
--             withFile "target.tex" WriteMode 
--             (\tex -> 
--                withFile "target.html" WriteMode
--                (\html -> do
--                    cnts <- System.IO.hGetContents src
--                    case parse (parseDoc :: ParsecT Dec String Identity Doc) "source.idoc" cnts of
--                      (Right x) -> do
--                        System.IO.hPutStr tex (Data.Text.unpack $ utRender x)
--                        System.IO.hPutStr html (renderPretty x)
--                      (Left err) -> System.IO.print err))))

-- | An example function.
main :: IO ()
main = (withFile "source.idoc" ReadMode 
         (\src ->
            withFile "target.tex" WriteMode 
            (\tex -> 
               withFile "target.html" WriteMode
               (\html -> do
                   cnts <- System.IO.hGetContents src
                   case parse (ils :: ParsecT Dec String Identity (Doc Main)) "source.idoc" cnts of
                     (Right x) -> do
                       --System.IO.hPutStr tex (Data.Text.unpack $ utRender x)
                       System.IO.hPutStr html (renderPretty x)
                     (Left err) -> System.IO.print err))))
         
