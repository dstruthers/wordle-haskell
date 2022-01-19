module Wordle where

import Control.Arrow ((&&&))
import Control.Monad (join)
import Data.Char (isAscii, isLower, toLower, toUpper)
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Set as Set

data Hint = Yes Char  -- character in word, in correct position
          | Move Char -- character in word, but incorrect position
          | No Char   -- character not in word
          deriving (Eq)

instance Show Hint where
  show h = case h of
    Yes c  -> fgGreen ++ [toUpper c] ++ resetColors
    Move c -> fgYellow ++ [toUpper c] ++ resetColors
    No c   -> [toUpper c]

    where fgGreen     = "\ESC[32m"
          fgYellow    = "\ESC[33m"
          resetColors = "\ESC[0m"

type Response = [Hint]

-- Given responses so far, indicate whether a word is a potential solution.
eligibleWord :: [Response] -> String -> Bool
eligibleWord responses word = isFiveLetters && isLowerCase && isOnlyAscii && meetsWordleRequirements
  where isFiveLetters           = length word == 5
        isLowerCase             = eachCharacter isLower
        isOnlyAscii             = eachCharacter isAscii
        meetsWordleRequirements = foldr (\r b -> checkResponse r word && b) True responses

        eachCharacter f         = foldr (&&) True $ map f word
        checkResponse resp word = checkYes resp word && checkNo resp word  && checkMove resp word
  
        checkYes (Yes c:cs) (c':cs') = c == c' && checkYes cs cs'
        checkYes (_    :cs) (c':cs') = checkYes cs cs'
        checkYes []         _        = True

        checkNo resp word = walk resp word
          where walk (No c:cs) word = Yes c `elem` resp || not (c `elem` word) && walk cs word
                walk (_   :cs) word = walk cs word
                walk []        _    = True

        checkMove resp word = walk resp word
          where walk (Move c:cs) (w:ws) = c /= w && c `elem` word && walk cs ws
                walk (_     :cs) (_:ws) = walk cs ws
                walk []          []     = True

-- Given a list of words, return a mapping of how frequently each letter of the alphabet occurs at least once in a word.
frequencies :: [String] -> Map Char Int
frequencies words = addScores $ map uniqueChars words
  where uniqueChars = foldr Map.union Map.empty . map (\c -> Map.singleton c 1)
        addScores   = foldr (Map.unionWith (+)) Map.empty

-- Given a list of words, return the word with the most frequently occurring letters.
bestGuess :: [String] -> String
bestGuess words =
  let freqs      = frequencies words
      score word = foldr (\c n -> n + Map.findWithDefault 0 c freqs) 0 (Set.fromList word)
      scoreMap   = map (id &&& score) words
  in fst $ foldr compareScoredWords ("", 0) scoreMap
  where compareScoredWords (w1, s1) (w2, s2)
          | s1 >= s2  = (w1, s1)
          | otherwise = (w2, s2)

-- Given responses so far, suggest a guess from the system dictionary file.
guess :: [Response] -> IO String
guess responses = readFile dict >>= return . bestGuess . filter (eligibleWord responses) . lines
  where dict = "/usr/share/dict/words"

-- Create a Response based on the provided solution and guess.
respond :: String -> String -> Response
respond solution guess = zipWith check solution guess
  where check s g
          | s == g            = Yes g
          | g `elem` solution = Move g
          | otherwise         = No g

-- Attempt to find the provided solution using up to 6 guesses.
play :: String -> IO ()
play solution = runGame [] (map toLower solution)
  where runGame responses solution = do
          g <- guess responses
          let r = respond solution g
          displayResponse r
          let guessCount = length responses + 1
          if success r
            then putStrLn $ "Success in " ++ show guessCount ++ "/" ++ show guessLimit
            else if guessCount == guessLimit
                 then putStrLn "Failed"
                 else runGame (responses ++ [r]) solution

        displayResponse = putStrLn . join . map show
        
        success (Yes _:rs) = success rs
        success []         = True
        success _          = False

        guessLimit = 6

-- Example usage
main :: IO ()
main = play "abbey"
