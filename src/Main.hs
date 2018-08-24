{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Main where

import Data.Algorithm.Diff (Diff(..))
import Data.Algorithm.Diff.Gestalt
import Data.Foldable
import Rainbow
import qualified Data.ByteString as BS
import qualified Data.List.Split as Split
import Control.Monad
import Data.List
import qualified Streaming as S
import qualified Streaming.Prelude as S
import Control.Error
import Data.Char
import Data.Function

-- Diffs two strings (by some word chunking function) and returns two colorized lines
diffSplitBy :: (String -> [String]) -> String -> String -> ([Chunk String], [Chunk String])
diffSplitBy f a b =
  let
    color (First s) = chunk s & fore red
    color (Second s) = chunk s & fore green
    color (Both s _) = chunk s & faint

    side (First s) = ([color (First (concat s))], [chunk $ map (const ' ') (concat s)])
    side (Second s) = ([chunk $ map (const ' ') (concat s)], [color (Second (concat s))])
    side (Both sa sb) = ([color (Both (concat sa) (concat sb))], [color (Both (concat sa) (concat sb))])
  in
    fold $ map side $ diff (f a) (f b)

-- Whether or not the line is of the form: 'some type' is not assignable to 'some other type'
hasTwoTypes :: String -> Bool
hasTwoTypes s = "assignable to" `isInfixOf` s

-- Prints TypeScript errors as pretty diffs
prettyTSError :: String -> IO ()
prettyTSError e = case Split.splitOn "'" e of
  [pre, a, hasTwoTypes -> True, b, post] -> do
    let
      (lineA, lineB) = diffSplitBy wordChunks a b
      indentation = map (const ' ') $ takeWhile (== ' ') pre
      putChunks = mapM_ BS.putStr . chunksToByteStrings toByteStringsColors256
    putStr indentation >> putChunks lineA >> putStrLn ""
    putStr indentation >> putChunks lineB >> putStrLn ""
  _ -> putStrLn e

data CharCategory = CSpace | CWord | COther deriving (Eq)

-- "foo: string" -> ["foo", ":", " ", "string"]
wordChunks :: String -> [String]
wordChunks string =
  let
    category char
      | isSpace char = CSpace
      | isAlpha char = CWord
      | otherwise = COther
  in
    groupBy ((==) `on` category) string

-- For development
ghci = S.readFile "ts-error.txt" $ S.mapM_ prettyTSError

-- Reads lines from stdin and converts one-line type errors into two-line diffs
main = S.mapM_ prettyTSError $ S.stdinLn
