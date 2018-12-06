{-# LANGUAGE NoImplicitPrelude #-}
-- Tasty makes it easy to test your code. It is a test framework that can
-- combine many different types of tests into one suite. See its website for
-- help: <http://documentup.com/feuerbach/tasty>.
--import qualified Test.Tasty
-- Hspec is one of the providers for Tasty. It provides a nice syntax for
-- writing tests. Its website has more info: <https://hspec.github.io>.
import ClassyPrelude
import Test.Tasty
import Test.Hspec
import Test.Tasty.Hspec

import Text.IDoc.ParseSpec

main :: IO ()
main = do
  print "hello"
  -- x <- parseTests
  -- print x
  test <- testSpec "idoc" parseSpec
  defaultMain test

spec :: Spec
spec = parallel $ do
    it "is trivially true" $ do
        True `shouldBe` True
