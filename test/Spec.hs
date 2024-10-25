{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- TODO: generate filenames in same way like using text generation too. hav ea filepath newtype. just three.... exclude keywords.
{- | Test suite for the search module.

The approach to testing the search functionality is to simplly test the ranking of the
search results, ensuring it ranks documents the way I'd desire and expect.

-}

module Main (main) where

import Bore.FileLayout
import Bore.SpacecookieClone.Search

import Control.Monad
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Text.IO qualified as TIO
import Network.Gopher
import System.FilePath
import System.IO.Temp (withSystemTempDirectory)
--import Test.DocTest (doctest)
import Test.QuickCheck
import qualified Data.Set as Set

defaultFileNames :: [FilePath]
defaultFileNames = ["all_keywords.txt", "sprinkled_keywords.txt", "no_keywords.txt"]

noKeywordsMessage :: Text
noKeywordsMessage = "This document does not contain any of the keywords we are searching for."

-- TODO:
-- needs to exclude filenames too! or intentionally match filename keywords and adjust for
-- such in the property! that would be a good test! could easily do this by using keyword
-- in filename and otherwise adding the filenames to nokeywords!
forbiddenKeywords :: [Text]
forbiddenKeywords = (T.words "This document does not contain any of the keywords we are searching for.") ++ (concatMap selectorWords defaultFileNames)

-- FIXME: put keywords into file name too but nothing in document, but still make that first.
-- Property to test if getSearchResults ranks documents as expected
prop_getSearchResultsRanking :: Keywords -> Property
prop_getSearchResultsRanking (Keywords keywords) = ioProperty $ do
  withSystemTempDirectory "test-search" $ \dir -> do
    -- Define file names and contents
    -- FIXME/TODO just add one keyword match. also how about a keyword in the file name?
    -- Fixme: ensure keyword safety with `isSafeFilename` somehow
    let
      fileNamedAfterKeywords = T.unpack $ T.intercalate "_" keywords <> ".txt"
      (fileNames :: [FilePath]) = [fileNamedAfterKeywords] ++ defaultFileNames
    let expectedResults = init fileNames  -- in that order, too
    let contiguousKeywords = T.unwords keywords
    -- what about SPARSE keywords coming after this?
    let sprinkledOrderedKeywords = "This is some text with " <> T.intercalate " and " keywords <> " scattered."
    -- could even do one that's out-of-order! but above should test proximity

    -- Write files with specified contents
    let fileContents = [noKeywordsMessage, contiguousKeywords, sprinkledOrderedKeywords, noKeywordsMessage]
    forM_ (zip fileNames fileContents) $ \(fileName, content) -> do
      TIO.writeFile (dir </> fileName) content

    -- Call getSearchResults
    response <- getSearchResults (T.unwords keywords) dir dir

    -- Extract file paths in the returned results (ordered by rank)
    let rankedFiles = case response of
          MenuResponse items -> [takeFileName (T.unpack (TE.decodeUtf8 selector)) | Item File _ selector _ _ <- items]
          _ -> []
    let items = case response of
          MenuResponse i -> i
          _ -> []
    putStrLn . show $ (zip fileNames fileContents)
    _ <- forM  (items) $ \f -> putStrLn . show $ f
    return $ rankedFiles === expectedResults

-- FIXME: use isValid to ensure filenames are safe, also should check length!
-- Generate random text data
instance Arbitrary Text where
  arbitrary = sized $ \n -> do
    -- Define the character generator (lowercase letters)
    let charGen = elements ['a'..'z']
    let forbiddenSet = Set.fromList forbiddenKeywords
    -- Create a generator for Text with the desired properties
    suchThat
      (do
        -- Generate a length between minLen and minLen + 10
        len <- choose (2, 2 + n)
        -- Generate a list of 'len' lowercase letters
        chars <- vectorOf len charGen
        -- Convert the list of chars into Text
        return $ T.pack chars
      )
      -- Predicate to ensure the generated Text is not in forbiddenWords
      (\txt -> txt `Set.notMember` forbiddenSet)

newtype Keywords = Keywords [Text]
  deriving (Show)

instance Arbitrary Keywords where
  arbitrary = sized $ \n -> do
    numKeywords <- chooseInt (2, min 10 (2 + n))  -- Generate between 2 and 10 keywords
    keywords <- vectorOf numKeywords arbitrary
    let combinedKeywords = T.unpack $ T.intercalate "_" keywords
    if isSafeFilename combinedKeywords
      then return $ Keywords keywords
      else arbitrary  -- Retry if the generated keywords don't form a valid filename

-- I know it looks weird, but I believe this is what has to happen.
-- See: https://hackage.haskell.org/package/QuickCheck-2.15.0.1/docs/Test-QuickCheck.html#v:quickCheckAll
return []

runTests :: IO Bool
runTests = $quickCheckAll

-- Run doctests independently
--runDoctests :: IO ()
--runDoctests = doctest ["src"]

-- Main function that runs both QuickCheck and Doctests
main :: IO ()
main = do
  --putStrLn "Running doctests..."
  --runDoctests
  
  putStrLn "Running QuickCheck properties..."
  allTestsPassed <- runTests
  if allTestsPassed
    then putStrLn "All tests passed."
    else putStrLn "Some tests failed."