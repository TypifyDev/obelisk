{-# LANGUAGE NoImplicitPrelude #-}
-- |
-- Module      : Scrappy.Files
-- Description : File system utilities for searching and analyzing files with scraping patterns
-- Copyright   : (c) Galen Sprout 2024
-- License     : BSD-3-Clause
-- Maintainer  : galen.sprout@gmail.com
module Scrappy.Files
  ( -- | @since 0.1.0.0
    listFilesRecursive
    -- | @since 0.1.0.0
  , searchFile
    -- | @since 0.1.0.0
  , searchStrFile
    -- | @since 0.1.0.0
  , searchManyFile
    -- | @since 0.1.0.0
  , countOccurrences
    -- | @since 0.1.0.0
  , areFilesUsed
  ) where

import Prelude
  ( Bool, IO, Int, Maybe(..), String, FilePath
  , ($), (<$>), (+)
  , return, concat, elem, filter, mapM, mconcat, mempty, not, print, pure
  , readFile
  )

import Control.Monad (forM)
import Data.List (foldl')
import qualified Data.Map.Strict as Map (Map, empty, insertWith, keys)
import System.Directory (doesDirectoryExist, listDirectory, makeAbsolute)
import System.FilePath (takeFileName, (</>))
import Text.Parsec (string)

import Scrappy.Elem.ElemHeadParse (buildElemsOpts)
import Scrappy.Scrape (ScraperT, exists, scrape)

-- | Recursively lists all files in a directory, returning absolute file paths.
--
-- Traverses the directory tree depth-first, collecting the absolute path of
-- every regular file encountered.
--
-- @since 0.1.0.0
listFilesRecursive :: FilePath -> IO [FilePath]
listFilesRecursive dir = do
    contents <- listDirectory dir
    paths <- forM contents $ \name -> do
        let fullPath = dir </> name
        isDir <- doesDirectoryExist fullPath
        if isDir
            then listFilesRecursive fullPath
            else do
                absPath <- makeAbsolute fullPath
                return [absPath]
    return (concat paths)

-- | Searches a file for the presence of a scraper pattern.
--
-- Returns 'True' if the pattern is found anywhere in the file contents.
--
-- @since 0.1.0.0
searchFile :: ScraperT a -> FilePath -> IO Bool
searchFile p fp = do
  str <- readFile fp
  pure $ exists p str

-- | Searches a file for the presence of a literal string.
--
-- Convenience wrapper around 'searchFile' using a 'string' parser.
--
-- @since 0.1.0.0
searchStrFile :: String -> FilePath -> IO Bool
searchStrFile s fp = searchFile (string s) fp

-- | Searches a file for multiple strings and counts their occurrences.
--
-- Parses the file contents looking for any of the given strings and returns
-- a map from each found string to its occurrence count.
--
-- @since 0.1.0.0
searchManyFile :: [String] -> FilePath -> IO (Map.Map String Int)
searchManyFile strs fp = do
  file <- readFile fp
  case scrape (buildElemsOpts strs) file of
    Nothing -> pure mempty
    Just results -> pure $ countOccurrences results

-- | Counts occurrences of each unique string in a list.
--
-- Returns a map from each distinct string to the number of times it appears.
--
-- @since 0.1.0.0
countOccurrences :: [String] -> Map.Map String Int
countOccurrences = foldl' (\acc word -> Map.insertWith (+) word 1 acc) Map.empty

-- | Checks which source files from one directory are referenced in another.
--
-- Lists all files in @sourceDir@, then searches all files in @usageDir@ for
-- occurrences of each source file name. Prints a usage map and the list of
-- source files that were not found in any usage file.
--
-- @since 0.1.0.0
areFilesUsed :: FilePath -> FilePath -> IO ()
areFilesUsed sourceDir usageDir = do
  sources <- listFilesRecursive sourceDir
  searchFiles <- listFilesRecursive usageDir
  let sources' = takeFileName <$> sources
  maps <- mapM (\x -> searchManyFile sources' x) searchFiles
  let mapped = mconcat maps
  print mapped
  print ("---" :: String)
  print $ filter (\s -> not $ elem s (Map.keys mapped)) sources'
