import PdePreludat
import Library
import Test.Hspec


main :: IO ()
main = hspec $ do

    describe "No se efectuo ningun testeo" $ do 

      it "1+1 es 2" $ do
            (1+1) `shouldBe` 2
