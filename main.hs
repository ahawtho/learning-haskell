{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import GHC.Generics
import System.Random
import Data.Aeson
import Data.List (length, nub, sortBy, group, minimumBy, maximumBy, sort, reverse)
import Text.Printf
import qualified Data.Vector as V
import Data.Vector ((!))
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Tuple.HT (mapFst)
import Data.Ord (comparing, Down(..))

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
    
parseFirstNames :: [T.Text] -> [T.Text]
parseFirstNames lines =
    concat $ map (((\x -> [(x !! 1), (x !! 3)]) . T.splitOn "\t")) lines
    
parseLastNames :: [T.Text] -> [T.Text]
parseLastNames lines =
    map (T.toUpper . T.toLower . (!! 0) . T.splitOn " ") lines

genPersonList :: RandomGen r => r -> V.Vector T.Text -> V.Vector T.Text -> [Person]
genPersonList rand0 firstNames lastNames =
    let
        genPerson' = genPerson firstNames lastNames
        init = genPerson' rand0
    in fst $ unzip $ take 100 $ iterate (\(_, randN) -> genPerson' randN) init

readPeople peopleFile = (decodeFileStrict peopleFile) :: IO (Maybe [Person])

analyze :: Int -> [Person] -> AnalysisResults
analyze topCount people = do
    AnalysisResults {
        uniqueFirstNames = length $ nub $ map firstName people,
        minAge = minimum $ map age people,
        maxAge = maximum $ map age people,
        oldest = take topCount $ sortBy (comparing (Down . age)) people,
        popularLastNames = take topCount
             $ sortBy (comparing (Down . snd))
             $ map (\l@(x:xs) -> (x, length l))
             $ group
             $ sort
             $ map lastName people
    }

printAnalysis results = do
    putStrLn $ printf "\nThere are %d distinct firstnames." $ uniqueFirstNames results
    putStrLn $ printf "Minimum age is %d." $ minAge results
    putStrLn $ printf "Maximum age is %d." $ maxAge results
    putStrLn $ printf "Top %d oldest people:" $ length $ oldest results
    mapM_ (putStrLn . show) $ oldest results
    putStrLn $ printf "\nTop %d most popular last names with frequency:" $ length $ popularLastNames results
    mapM_ (putStrLn . show) $ popularLastNames results

genPerson :: RandomGen r => V.Vector T.Text -> V.Vector T.Text -> r -> (Person, r)
genPerson firstNames lastNames rand0 =
    let (randFirst, rand1) = mapFst (firstNames !) $ randomR (0, V.length firstNames - 1) rand0
        (randLast, rand2) = mapFst (lastNames !) $ randomR (0, V.length lastNames - 1) rand1
        (randAge, rand3) = randomR (1, 100) rand2
    in (Person {
           firstName = randFirst,
           lastName = randLast,
           age = randAge
        }, rand3)