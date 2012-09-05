{-# LANGUAGE ScopedTypeVariables #-}
module Main (main) where

import Control.Applicative
import Control.Monad
import qualified Data.ByteString.Lazy as BL
import Data.Csv
import Data.Char
import Data.Time.Calendar
import Data.Time.LocalTime
import qualified Data.Vector as V
import System.Environment (getArgs, getProgName)

main :: IO ()
main = do
  args <- getArgs
  if null args
      then do
          progName <- getProgName
          putStrLn $ "usage:  " ++ progName ++ " <csvfile>"
      else bloodprint $ head args

bloodprint :: FilePath -> IO ()
bloodprint filename = do
  let removeLine = BL.tail . BL.dropWhile (/= fromIntegral (ord '\n'))
      removeThreeLines = removeLine . removeLine . removeLine
  csv <- fmap removeThreeLines $ BL.readFile filename
  case decodeWith (defaultDecodeOptions {
                     decDelimiter = fromIntegral (ord ';')
                   }) csv of
      Left err -> putStrLn err
      Right v -> V.forM_ v $ \  bpmRaw ->
        print $ bpmRawToBpm bpmRaw

data BloodPressureMeasurementRaw = BPMRaw
    { date :: String
    , time :: String
    , sys :: Integer
    , dia :: Integer
    , pul :: Integer
    }

instance FromRecord BloodPressureMeasurementRaw where
    parseRecord v
        | V.length v >= 5 = BPMRaw <$>
                          v .! 0 <*>
                          v .! 1 <*>
                          v .! 2 <*>
                          v .! 3 <*>
                          v .! 4
        | otherwise     = mzero

data BloodPressureMeasurement = BPM
    { datetime :: LocalTime
    , systolic :: Integer
    , diastolic :: Integer
    , pulse :: Integer
    }
    deriving Show

bpmRawToBpm :: BloodPressureMeasurementRaw -> BloodPressureMeasurement
bpmRawToBpm (BPMRaw d t sy di pu) =
        BPM mkLocalTime sy di pu
  where mkLocalTime :: LocalTime
        mkLocalTime = LocalTime
          (fromGregorian (read (d' !! 2)) (read (d' !! 1)) (read (head d')))
          (TimeOfDay (read (head t')) (read (t' !! 1)) (read (t' !! 2)))
        breaks :: Char -> String -> [String]
        breaks c [] = []
        breaks c str = let (f, r') = break (==c) str
                       in f : breaks c (tail r')
        d' = breaks '.' d
        t' = breaks ':' t
