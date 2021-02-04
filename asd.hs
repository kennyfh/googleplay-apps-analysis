-- import Text.CSV -- Implementación de csv en Haskell
{-# LANGUAGE OverloadedStrings #-}


import Control.Applicative
import qualified Data.ByteString.Lazy as BL
import Data.Csv
import qualified Data.Vector as V

data Person = Person
    { name   :: !String
    , salary :: !Int
    , days :: !Int
    }

instance FromNamedRecord Person where
    parseNamedRecord m = Person <$> m .: "name" <*> m .: "salary" <*> m .: "days"



main :: IO ()
main = do
    csvData <- BL.readFile "test.csv"
    case decodeByName csvData of
        Left err -> putStrLn err
        Right (_, v) -> V.forM_ v printElement
        -- Right (_, v) -> print $ V.toList $ V.map name v
        -- Right (_, v) -> print $ V.toList $ V.map valuesToList v



printElement p = putStrLn $ name p ++ " earns " ++ show (salary p) ++ " dollars " ++ show (days p) ++ " DAYS"