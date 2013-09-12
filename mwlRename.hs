module Main where

import Text.ParserCombinators.Parsec
import System.Environment
import System.Directory
import qualified Data.Map as Map
import Control.Monad
import Data.Maybe (fromJust)

type OldFilename = String
type NewFilename = String
type Extension   = String
type Amp         = String -- length 2  eg a1
type AmpDay      = String -- length 2  eg a112 for a top, Nov 12th
data Trode = Trode { unTrode :: Int }


main :: IO ()
main = do
  nmF <- readFile "mapping.txt"
  let (Right nameMap) = parse parseMapping "mapping file" nmF
  fromDirectory "raw" $ do
    fNames <-(getDirectoryContents ".")
    mapM_ (rename nameMap) (filter (\f -> canBeTranslated nameMap f && needsTranslating f) fNames)

rename :: Map.Map String Trode -> OldFilename -> IO ()
rename nameMap fn = let newName = translate nameMap fn in
  renameFile fn (translate nameMap fn) >> putStrLn (unwords ["Renaming", fn,"->",newName])

instance Show Trode where
  show (Trode i) = if i < 10 then '0' : show i else show i

-- Should be using bracket here
fromDirectory :: FilePath -> IO a -> IO a
fromDirectory path act = do
  baseDir <- getCurrentDirectory
  setCurrentDirectory path
  r <- act
  setCurrentDirectory baseDir
  return r

needsTranslating :: OldFilename -> Bool
needsTranslating fn@(f:_) = length fn > 4 &&
                            f >= 'a' && f <= 'z' && -- starts alphabetical
                                  e == 's'          -- extn starts with 's'
  where (e:_) = fnExtension fn 

canBeTranslated :: Map.Map String Trode -> OldFilename -> Bool
canBeTranslated trodeMap fn =
  case oldNameToParts fn of
    Nothing -> False
    Just (a1,a2,_) ->
      case (Map.member (take 2 a1) trodeMap, Map.member (take 2 a2) trodeMap) of
        (True,True) -> True
        _           -> False

translate :: Map.Map String Trode -> OldFilename -> NewFilename
translate m oFn = concat [newA,newB,".",e]
  where Just (oldA, oldB, e) = oldNameToParts oFn
        oldTrodeToNew :: String -> String
        oldTrodeToNew part = (show . fromJust . (\k -> Map.lookup k m) $ (take 2 part)) ++ drop 2 part 
        (newA,newB) = (oldTrodeToNew oldA, oldTrodeToNew oldB)

oldNameToParts :: OldFilename -> Maybe (AmpDay, AmpDay, Extension)
oldNameToParts name
  | length name /= 12 = Nothing -- Must be 4 chars, 4 chars, dot, 3 chars
  | otherwise = Just (firstHalf, secondHalf, extn)
  where firstHalf  = take 4 name
        secondHalf = take 4 . drop 4 $ name
        extn       = fnExtension name

fnExtension :: OldFilename -> String
fnExtension = drop 1 . dropWhile (/= '.')
        
parseMapping :: CharParser () (Map.Map String Trode)
parseMapping = return . Map.fromList =<< parseLine `endBy` (char '\n')

parseLine :: CharParser () (String, Trode)
parseLine = do
  amp <- many alphaNum
  char ' ' <|> char '\t'
  trd <- many alphaNum
  return (amp, Trode (read trd))
