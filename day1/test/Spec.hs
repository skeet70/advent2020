import           Universum
import           Test.Hspec
import           Lib

main :: IO ()
main = hspec $ do
  describe "findSum" $ do
    it "finds two entries in a list that sum to the example target value" $ do
      let expenses = [1721, 979, 366, 299, 675, 1456]
      findSum 2 2020 expenses `shouldBe` [1721, 299]
    it "finds three entries in a list that sum to the example target value" $ do
      let expenses = [1721, 979, 366, 299, 675, 1456]
      findSum 3 2020 expenses `shouldBe` [979, 366, 675]

  describe "multipliedSum" $ do
    it "finds the multple of two entries in a list that sum to the target" $ do
      let expenses = [1721, 979, 366, 299, 675, 1456]
      multipliedSum 2 2020 expenses `shouldBe` (514579)

    it "finds the multiple of three entries in a list that sum to the target"
      $ do
          let expenses = [1721, 979, 366, 299, 675, 1456]
          multipliedSum 3 2020 expenses `shouldBe` (241861950)

    it "returns the right answer for the first half of the problem" $ do
      contents <- readFile "./resources/input.txt"
      let expenses = rights $ fmap readEither (lines contents)
      multipliedSum 2 2020 expenses `shouldBe` (691771)

    it "returns the right answer for the second half of the problem" $ do
      contents <- readFile "./resources/input.txt"
      let expenses = rights $ fmap readEither (lines contents)
      multipliedSum 3 2020 expenses `shouldBe` (232508760)

