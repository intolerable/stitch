module Stitch.Types.SelectorSpec where

import Stitch.Types.Selector

import Data.Monoid (Monoid(..))
import Data.Semigroup ((<>))
import Test.Hspec hiding (Selector)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do

  describe "Selector" $ do
    describe "Monoid" $ do
      it "should hold that x <> mempty == x" $ do
        let smempty = mempty :: Selector
        "h1" <> smempty `shouldBe` "h1"
        ("h1" <> "h2") <> smempty `shouldBe` "h1 h2"
        "&:hover" <> smempty `shouldBe` "&:hover"
      it "should hold that mempty <> x == x" $ do
        let smempty = mempty :: Selector
        smempty <> "h1" `shouldBe` "h1"
        smempty <> ("h1" <> "h2") `shouldBe` "h1 h2"
        smempty <> "&:hover" `shouldBe` "&:hover"
      it "should hold that x <> (y <> z) == (x <> y) <> z" $ do
        let test x y z = (x <> (y <> z) :: Selector) `shouldBe` ((x <> y) <> z)
        test "h1" mempty "&:hover"
        test "h1" "h2" "h3"
        test mempty mempty mempty
        test "&:active" "&:hover" ".red &"
    describe "fromText" $ do
      it "should ignore empty strings" $ do
        fromText "" `shouldBe` Selector []
      it "should handle commas" $ do
        fromText "h1, h2" `shouldBe` Selector ["h1", "h2"]
