module Lib where

import           Lib.Prelude
import qualified Data.Text                     as T
import qualified Universum.Unsafe              as Unsafe

data Rule = Rule Int Int Char deriving Show

countValid :: Text -> Int
countValid rulesAndPasswords = length . filter (== True) $ fmap
  (checkLine checkRulePassword)
  (lines rulesAndPasswords)

minMaxCharacter :: [Text] -> Maybe Rule
minMaxCharacter [minChars, maxChars, character] = do
  minChars' <- rightToMaybe $ readEither minChars
  maxChars' <- rightToMaybe $ readEither maxChars
  let character' = T.head character
  Just $ Rule minChars' maxChars' character'
minMaxCharacter _ = Nothing

split :: Text -> [Text]
split t = T.splitOn ": " t >>= words >>= T.splitOn "-"

checkRulePassword :: Rule -> Text -> Bool
checkRulePassword (Rule minChars maxChars character) password =
  charCount >= minChars && charCount <= maxChars
  where charCount = T.length $ T.filter (== character) password

checkLine :: (Rule -> Text -> Bool) -> Text -> Bool
checkLine checkingF line = fromMaybe False
  $ map (\rule' -> checkingF rule' password) rule
 where
  line'    = split line
  rule     = minMaxCharacter $ take 3 line'
  password = Unsafe.head $ drop 3 line'

checkRulePasswordOTCA :: Rule -> Text -> Bool
checkRulePasswordOTCA (Rule firstIndex secondIndex character) password =
  ((T.index password fI') == character) /= ((T.index password sI') == character)
 where
  fI' = firstIndex - 1
  sI' = secondIndex - 1

countValidOTCA :: Text -> Int
countValidOTCA rulesAndPasswords = length . filter (== True) $ fmap
  (checkLine checkRulePasswordOTCA)
  (lines rulesAndPasswords)
