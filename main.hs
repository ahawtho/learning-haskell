{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import GHC.Generics
import System.Random
import Data.Aeson
import Data.List (length, nub, sortOn, group, minimumBy, maximumBy, sort, reverse)
import Text.Printf
import qualified Data.Vector as V
import Data.Vector ((!))
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

data Person = Person {
    firstName :: T.Text,
    lastName  :: T.Text,
    age       :: Int
} deriving (Generic, Show)

data AnalysisResults = AnalysisResults {
    uniqueFirstNames    :: Int,
    minAge              :: Int,
    maxAge              :: Int,
    oldest              :: [Person],
    popularLastNames    :: [(T.Text, Int)]
}

instance ToJSON Person where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON Person

firstNamesFile = "data/firstNames.txt"
lastNamesFile = "data/lastNames.txt"
peopleFile = "data/people.txt"
topCount = 3

main = do
    rand <- getStdGen
    firstNameContent <- TIO.readFile firstNamesFile
    lastNameContent <- TIO.readFile lastNamesFile
    let firstNames = V.fromList $ parseFirstNames $ T.lines firstNameContent
    let lastNames = V.fromList $ parseLastNames $ T.lines lastNameContent
    encodeFile peopleFile $ genPersonList rand firstNames lastNames
    maybePeople <- readPeople peopleFile
    let results = fmap (analyze topCount) maybePeople 
    case results of
        Nothing -> putStrLn ("Error reading file: " ++ peopleFile)
        Just results -> printAnalysis results

printAnalysis results = do
    putStrLn $ printf "\nThere are %d distinct firstnames." $ uniqueFirstNames results
    putStrLn $ printf "Minimum age is %d." $ minAge results
    putStrLn $ printf "Maximum age is %d." $ maxAge results
    putStrLn $ printf "Top %d oldest people:" $ length $ oldest results
    mapM_ (putStrLn . show) $ oldest results
    putStrLn $ printf "\nTop %d most popular last names with frequency:" $ length $ popularLastNames results
    mapM_ (putStrLn . show) $ popularLastNames results

readPeople peopleFile = (decodeFileStrict peopleFile) :: IO (Maybe [Person])

analyze :: Int -> [Person] -> AnalysisResults
analyze topCount people = do
    AnalysisResults {
        uniqueFirstNames = length $ nub $ map firstName people,
        minAge = minimum $ map age people,
        maxAge = maximum $ map age people,
        oldest = take topCount $ reverse $ sortOn age people,
        popularLastNames = take topCount
             $ reverse
             $ sortOn snd
             $ map (\l@(x:xs) -> (x, length l))
             $ group
             $ sort
             $ map lastName people
    }
    
parseFirstNames :: [T.Text] -> [T.Text]
parseFirstNames lines =
    concat $ map (((\x -> [(x !! 1), (x !! 3)]) . T.splitOn "\t")) lines
    
parseLastNames :: [T.Text] -> [T.Text]
parseLastNames lines =
    map (T.toUpper . T.toLower . (!! 0) . T.splitOn " ") lines

genPersonList :: RandomGen r => r -> V.Vector T.Text -> V.Vector T.Text -> [Person]
genPersonList random firstNames lastNames =
    let randSelector = genRandomRangesList [(0, V.length firstNames - 1), (0, V.length lastNames - 1), (1, 100)]
    in map (genPerson firstNames lastNames) (take 100 $ randSelector random)

genPerson :: V.Vector T.Text -> V.Vector T.Text -> [Int] -> Person
genPerson firstNames lastNames (nextFirstNameId:nextLastNameId:nextAge:[]) =
    Person {
        firstName = firstNames ! nextFirstNameId,
        lastName = lastNames ! nextLastNameId,
        age = nextAge
     }

genRandomRangesList :: RandomGen r => [(Int, Int)] -> r -> [[Int]]
genRandomRangesList ranges r =
    let init = genRandomRanges ranges r
    in map fst $ iterate (\(_, r) -> genRandomRanges ranges r) init
    
genRandomRanges :: RandomGen r => [(Int, Int)] -> r -> ([Int], r)
genRandomRanges ranges r =
    case ranges of
        [] -> ([], r)
        rg:rgs -> let (x, r1) = randomR rg r
                      (xs, r2) = genRandomRanges rgs r1
                  in (x:xs, r2)