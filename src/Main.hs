{-# LANGUAGE NoMonoLocalBinds #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Main where

import Data.Char
import Data.Either
import Data.Function
import Data.List
import Safe
import System.IO.Unsafe

-- import Data.Map (Map)

-- import qualified Data.Map as Map
-- import Data.Bool (bool)

-- https://new.wordsmith.org/anagram

main :: IO ()
main = putStrLn "big beats are the best"

-- | use infix
without :: String -> String -> Maybe String
without s = foldr (\c u -> removeOne c =<< u) (Just s)

removeOne :: Char -> String -> Maybe String
removeOne x = \case
    [] -> Nothing
    c : cs -> if c == x then Just cs else (c :) <$> removeOne x cs

checkAnagram :: String -> String -> Bool
checkAnagram = (==) `on` (sort . filter isAlpha)

checkAll :: Bool
-- checkAll = and [all (checkAnagram s) ans | (s, ans) <- allAnagrams]
checkAll = null checkAll'

checkAll' :: ([[(String, String)]])
checkAll' = filter (not . null) [map (s,) $ filter (not . checkAnagram s) ans | (s, ans) <- allAnagrams]

-- newtype Sorted a = Sorted [a]

-- sorted :: Ord a => [a] -> Sorted a
-- sorted = Sorted . sort

findWords :: [String] -> String -> [String]
findWords d s = findWords' (filter (all isAlpha) d) (sort s)

-- | Input string should be sorted
findWords' :: [String] -> String -> [String]
findWords' d = \case
    c : cs ->
        let ( match -- begin with c
                , rest -- don't begin with c
                ) = span ((== Just c) . headMay) $ dropWhile ((/= Just c) . headMay) d
         in do
                -- a <- match
                -- case a of
                --     [] -> return [c]
                --     a' -> _
                --TODO ooh but, if there's an empty, it's always first
                let (emptys, ws) = partitionEithers $ map (maybeToEither () . tailMay) match
                w <- findWords ws cs
                let w' = c : w
                return w'
    -- ++ if null emptys then [] else [""]
    -- ++ findWords rest cs
    -- ++ if [] `elem` match then [[c]] else []
    [] -> case d of
        [] : _ -> [[]]
        _ -> []

maybeToEither :: a -> Maybe b -> Either a b
maybeToEither x = maybe (Left x) Right

{-# NOINLINE dict #-}
dict :: [String]
dict = lines $ unsafePerformIO $ readFile "/usr/share/dict/words"

-- data Dict
--     = Empty
--     | Dict (Map Char (Bool, Dict))
--     deriving (Show)

-- allWords :: Dict -> [String]
-- allWords = \case
--     Empty -> []
--     Dict m ->
--         let l = Map.toList m
--         in do
--             (c,(b,d)) <- l
--             applyWhen b (pure c :) $ map (c:) $ allWords d

-- applyWhen :: Bool -> (a -> a) -> a -> a
-- applyWhen = flip $ bool id

-- toyDict :: Dict
-- toyDict =
--     Dict $
--         Map.fromList
--             [ ( 'a',
--                 ( True,
--                   Dict $
--                       Map.fromList
--                           [ ( 'n',
--                               ( True,
--                                 Empty
--                               )
--                             )
--                           ]
--                 )
--               )
--             ]

-- -- this assumes the input list is alphabeticaly sorted, and ignores 'words' with punctuation
-- mkDict :: [String] -> Dict
-- mkDict = \case
--     c : cs ->
--         let (a,b) = span ((== head c) . head) cs
--         in  _

allAnagrams :: ([(String, [String])])
allAnagrams =
    [ ( "manchester united"
      , [ "shite name, red cunt"
        , "mu, ten cheats in red"
        , "ten in red. such team." --
        ]
      )
    , ( "stoke city"
      , ["sticky toe"] --
      )
    , ( "brighton and hove albion"
      , [ "dabbling in hot rave? oh no!"
        , "a hiving london bathrobe"
        , "behold! raving onion bath"
        , "hold thine raving baboon" --
        ]
      )
    , ( "nottingham forest"
      , ["fat hogs in torment"]
      )
    , ( "manchester city"
      , ["synthetic cream"]
      )
    , ( "tottenham hotspur"
      , [ "mount the posh tart"
        , "on to the trump hats" --
        , "trump has ten tooth"
        ]
      )
    , ( "wigan athletic"
      , ["what giant lice"]
      )
    , ( "crystal palace"
      , ["let's cry alpaca"] --
      )
    , ( "newcastle united"
      , ["wet dense lunatic"]
      )
    , ( "west bromwich albion"
      , [ "wow! monarchist bible"
        , "wow! moist bible ranch"
        , "bible racism - who won't?" --
        , "woman with blob cries"
        ]
      )
    , ( "wycombe wanderers"
      , [ "beware my nerd cows" --
        ]
      )
    , ( "shrewsbury town"
      , [ "newsworthy rubs" --
        ]
      )
    , ( "leicester city"
      , [ "electric yetis" --
        ]
      )
    , ( "newport county"
      , [ "cunt poetry now!"
        , "coy punter town"
        , "puny rotten cow" --
        ]
      )
    ]
