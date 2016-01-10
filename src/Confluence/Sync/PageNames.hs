module Confluence.Sync.PageNames (
  TitleCaseConversion(..)
, prettifyPageName
) where

import           Data.Char (toLower, toTitle, isUpper)
import           Data.List (find)
import           Data.String.Utils (replace)

import           System.FilePath

-- | Strategy for converting a file name into a nice page name.
data TitleCaseConversion =
  -- Automatically capitalise the first letter of each word
  TitleCase |
  -- Only applies title casing if there are no capitals in the word
  -- (otherwise preserves the capitalisation on a per word basis)
  PreserveIfContainsCapitals
  deriving (Show, Eq)

-- | Takes the filename of a page and sanitizes it for a Confluence page name.
--   Basically removes common symbols and capitalises each word.
prettifyPageName :: TitleCaseConversion -> String -> String
prettifyPageName strategy path = (unwords . map (capitaliseWord strategy) . words) $ (foldr replaceCharacter (takeBaseName path) [
      ("-", " ")
    , ("_", " ")
  ])
  where replaceCharacter :: (String, String) -> String -> String
        replaceCharacter (needle, replaced) str = replace needle replaced str

capitaliseWord :: TitleCaseConversion -> String -> String
capitaliseWord TitleCase [] = []
capitaliseWord TitleCase (c : cs) = toTitle c : map toLower cs
capitaliseWord PreserveIfContainsCapitals word =
  case (find isUpper word) of
    Just _  -> word
    Nothing -> capitaliseWord TitleCase word
