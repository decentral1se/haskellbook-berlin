import Test.Hspec
import Test.QuickCheck
import Data.Maybe (isJust)

import Lib

instance Arbitrary a => Arbitrary (List a) where
    arbitrary = fmap fromList arbitrary


main :: IO ()
main = hspec $ do
    describe "fromList" $ do
        it "converts []" $ do
            fromList [] `shouldBe` (Empty :: List Int)
    describe "showOurList" $ do
        it "shows simple example lists" $ do
            showOurList (Cons 'a' Empty) `shouldBe` "['a']"
    describe "fromList and show" $ do
        it "shows as usual" $ do
            property $ \list ->
                show list == show (fromList list :: List Int)

    describe "safeHead" $ do
        it "gives Nothing on Empty" $ do
            safeHead Empty `shouldBe` (Nothing :: Maybe String)
        it "gives first element" $ do
            property $ \ownList ->
                ownList /= Empty ==> isJust (safeHead ownList :: Maybe Char)
        it "gives proper first element" $ do
            property $ \e ownList ->
                safeHead (Cons e ownList) == Just (e :: String)
