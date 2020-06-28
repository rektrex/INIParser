{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad
import Data.Map (Map)
import Data.Text (Text)
import Data.Void
import Data.Char
import System.Environment (getArgs)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Data.Map as M
import qualified Data.Text.IO as TIO
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void Text

sc :: Parser ()
sc = L.space
  space1
  (L.skipLineComment ";")
  empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

newtype Header = Header Text deriving (Eq, Ord, Show)

pHeader :: Parser Header
pHeader = Header <$> (between (char '[') (char ']') $ takeWhile1P Nothing isAlphaNum)

type Name = Text
type Value = Text
type Assignment = (Name, Value)
type Assignments = Map Name Value

data Section = Section Header Assignments deriving (Eq, Show)

newtype Config = Config (Map Header Assignments) deriving (Eq, Show)

pAssignment :: Parser Assignment
pAssignment = do
  void space
  name <- takeWhile1P Nothing isAlphaNum
  void space
  void $ char '='
  void space
  val <- takeWhile1P Nothing isAlphaNum
  return (name, val)

pSection :: Parser Section
pSection = do
  header <- lexeme pHeader
  assignments <- many $ lexeme pAssignment
  return $ Section header (M.fromList assignments)

rollup :: Section
       -> Map Header Assignments
       -> Map Header Assignments
rollup (Section h a) m = M.insert h a m

pConfig :: Parser Config
pConfig = do
  void sc
  sections <- many $ lexeme pSection
  let mapOfSections = foldr rollup M.empty sections
  return $ Config mapOfSections

main :: IO ()
main = do
  args <- getArgs
  file <- TIO.readFile $ args !! 0
  parseTest pConfig file
