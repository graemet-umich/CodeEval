-- Levenshtein Distance

-- Modules absent from CodeEval:
-- Data.List.Split


--   I tested this code on the first 2000 lower case
-- apostrophe-excluded words from the 51175 word
-- american-english-small dictionary (a, aardvark, ... atmospheric,
-- atom), with test words (a, b) which took 40 seconds. They share the
-- same social network, and the sizes were 167 and 168, because a is
-- in the dictionary and b is not.
--   So why did 15-30 test words on a 10,000 word dictionary take just
-- over 1/2 second at CodeEval? Supercomputer? No.
--   My test dictionary was very dense, generating large social
-- networks. The CodeEval dictionary was likely sparse, generating
-- much smaller social networks.
--   The algorithm I used (see below) requires one pass over candidate
-- friends in the dictionary for each member of the social network. 

-- Suggested Improvements
-- Cache the social networks. This way if two test words share a
-- social network, it can be found quickly. Expect better results for
-- dense dictionaries that give rise to larger social networks.


import Data.Array (Array, (!), listArray)
import Data.List (foldl', groupBy, sortOn)
import qualified Data.Ord as Ord
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import System.Environment (getArgs)

main :: IO ()
main = do
  [iFile] <- getArgs
  input <- readFile iFile
  putStr $ solve input


-- CodeEval: 657 ms, 85521 B, 96.511
solve :: String -> String
solve fLines = unlines .
               map (show . socialNetworkOfWord dict) $  theWords
    where (theWords, dict) = parseFile . lines $ fLines

parseFile :: [String] -> ([String], Array Int [String])
parseFile fLines = (theWords, theDict)
    where theWords = takeWhile (/= "END OF INPUT") fLines
          theDict'  = tail $ dropWhile (/= "END OF INPUT") fLines
          theDict = wordsByWordLength theDict'
                 
{-
split :: Char -> String -> [String]
split cSplit s = (fst p) : (snd p)
    where p = foldr spl ("", []) s
          spl c (subStr, subStrs)
              | c == cSplit = ("", subStr:subStrs)
              | otherwise = (c:subStr, subStrs)
-}


-- sort words by word length
-- import Data.Function (on)
on :: (b -> b -> c) -> (a -> b) -> a -> a -> c  
f `on` g = \x y -> f (g x) (g y)
-- > groupBy (on (==) length) . sortOn length $ ["a", "aaa", "aa", "aa"]
-- [["a"],["aa","aa"],["aaa"]]


-- Don't calculate the Levenshtein distance between strings s1 and s2,
-- which is wasteful. Just calculate if the distance is 1, i.e. s1 and
-- s2 are friends.
-- Implementation detail:
--   All descriptions of the Levenshtein distance I visited allow the
-- length of s1 to be greater than, equal to, or less than the length
-- of s2. For clarity for those coming from those descriptions, I
-- don't order the lengths of s1 and s2 either, and that's reflected,
-- for example, in the triple-recursive call in the last three lines.
--   Forcing the length of s1 >= length of s2 would make the code
-- shorter, and some might say less smelly, but it obscures parallels
-- to the literature presentation. 
isFriend :: String -> String -> Bool
isFriend s1 s2 = isLevDist1 0 s1 s2 where
    isLevDist1 :: Int -> String -> String -> Bool
    isLevDist1 levDist _ _
        | levDist > 1 = False
    isLevDist1 levDist [] []
        | levDist == 1 = True
        | otherwise = False  -- s1 == s2, you can't be your neighbor
    isLevDist1 levDist [] s2
        | levDist == 0 && length s2 == 1 = True
        | otherwise = False
    isLevDist1 levDist s1 []
        | levDist == 0 && length s1 == 1 = True
        | otherwise = False
    isLevDist1 levDist (c1:c1s) (c2:c2s)
        | c1 == c2 = isLevDist1 levDist c1s c2s
        -- mismatch cases for lengths s1 > s2, s1 < s2, s1 == s2
        | otherwise = isLevDist1 (levDist + 1) c1s (c2:c2s) ||
                      isLevDist1 (levDist + 1) (c1:c1s) c2s ||
                      isLevDist1 (levDist + 1) c1s c2s 

--   To find friends, you know the difference in the lengths between
-- two friends is either -1, 0, or +1, which corresponds to a single
-- deletion, substitution, or insertion. Given the entire dictionary,
-- group it by dictionary word length triples. For example, at index
-- 1, put all words of lengths 1 and 2. At index 2, put all words of
-- lengths 1, 2, and 3.
--   In this way, if you consider a word of length i, then the only
-- friends it could have are found at group triple index i. This is
-- implemented as
--     candidateFriends = dict!fLen
-- in socialNetworkOfWord.
wordsByWordLength :: [String] -> Array Int [String]
wordsByWordLength dictionary = wbwl
    where maxLen = length . head . last $ grpSort
          grpSort = groupBy (on (==) length) .
                    sortOn length $ dictionary
          fullGrpSort = insertMissing grpSort
          tripGrpSort = zipWith3
                        (\a b c -> a ++ b ++ c)
                        fullGrpSort
                        (tail fullGrpSort)
                        (tail . tail $ (fullGrpSort ++ [[]] ))
          wbwl = listArray (1,maxLen) tripGrpSort

-- The strings at an index base 1 are the length of the strings. If
-- there are missing string lengths, fill them in.
-- For example, if there are no strings of length 2:
--   [["a","b"],["aaa"]] -> [["a","b"],[],["aaa"]]
insertMissing :: [[String]] -> [[String]]
insertMissing sss = [] : snd result
    where result = foldl' f (1, []) sss
          f (i, sss') ss
              | i == (length . head) ss = (i + 1, sss' ++ [ss])
              | otherwise = f (i + 1, sss' ++ [[]]) ss

--   Given the "grouped" dictionary (see wordsByWordLength above), find
-- the size of the social network of a given word.

--   The two datastructures are a set that contains the friends in the
-- social network and a queue (using either a sequence or a list; they
-- were the same speed) that holds the friends (of those already in
-- the social network set) whose friends we have yet to find.
--   The algorithm:
-- 0. Start with the empty set and the queue holding the given word.
-- 1. Pop friend1 off the queue and put her in the set.
-- 2. Get from the dictionary all candidate friends of friend1 based
--    on the length of friend1.
-- 3. Filter the candidates to get all friends of friend1.
-- 4. Remove all friends of friend1 already in the social network set;
--    these are new friends not seen previously.
-- 5. Push the new friends onto the queue.
-- 6. Recur to step 1.
socialNetworkOfWord :: Array Int [String] -> String -> Int
socialNetworkOfWord dict word =
    Set.size $ getSocNet Set.empty (Seq.singleton word) where
        
    getSocNet socNet allFriends
        | Seq.null allFriends = socNet
        | otherwise = getSocNet (Set.insert friend socNet)
                      friendsAndNewFriends 
        where (friend Seq.:< friends) = Seq.viewl allFriends
              fLen = length friend
              candidateFriends = dict!fLen
              friends' = filter (isFriend friend) candidateFriends
              newFriends = filter (flip Set.notMember socNet)
                           friends'
              friendsAndNewFriends = friends Seq.>< (Seq.fromList newFriends)
{-
socialNetworkOfWord dict word = Set.size $
                                getSocNet Set.empty [word] where
    getSocNet socNet [] = socNet
    getSocNet socNet (friend:friends) =
        getSocNet (Set.insert friend socNet)
                      friendsAndNewFriends
        where fLen = length friend
              candidateFriends = dict!fLen
              friends' = filter (isFriend friend) candidateFriends
              newFriends = filter (flip Set.notMember socNet)
                           friends'
              friendsAndNewFriends = friends ++ newFriends
-}
                           
                                 
