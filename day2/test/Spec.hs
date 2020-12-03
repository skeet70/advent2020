import           Universum
import           Test.Hspec
import           Lib

main :: IO ()
main = hspec $ do
  describe "countValid" $ do
    it "counts blessed valid passwords" $ do
      let passwords = "1-3 a: abcde\n1-3 b: cdefg\n2-9 c: ccccccccc"
      countValid passwords `shouldBe` 2

    it "counts valid passwords from test input" $ do
      passwords <- readFile "./resources/input.txt"
      countValid passwords `shouldBe` 564

  describe "countValidOTCA" $ do
    it "counts blessed valid passwords" $ do
      let passwords = "1-3 a: abcde\n1-3 b: cdefg\n2-9 c: ccccccccc"
      countValidOTCA passwords `shouldBe` 1

    it "counts valid passwords from test input" $ do
      passwords <- readFile "./resources/input.txt"
      countValidOTCA passwords `shouldBe` 325
