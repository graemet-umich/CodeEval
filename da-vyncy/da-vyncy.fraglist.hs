-- Da Vyncy

-- This challenge is O(n^2) over the number of fragments. At each
-- step, compare all pairs of fragments for longest overlap.

-- The algorithm states that of the remaining fragments, combine the
-- two fragments with the longest overlap.

-- If none of the remaining fragments overlap, the longest remaining
-- fragment is the original document.

import Data.List (maximumBy, minimumBy, sortOn, tails)
import Data.Ord (comparing)
import System.Environment (getArgs)
--import Debug.Trace

main :: IO ()
main = do
  [iFile] <- getArgs
  input <- readFile iFile
  putStr $ solve input


-- CodeEval: 4082 ms, 3739648 B, 70.674  (sortOn length)
--           4272 ms, 3735552 B, 69.734  (minimumBy (comparing length))
-- These are big texts with large fragments of length [2,1022].
solve :: String -> Fragment
solve = unlines .
        map (head . updateFragments . parseLine) .
        lines


type Fragment   = String        
type Fragments  = [String]    

-- fragment1 comes before fragment2 in the list of fragments.
data Overlap = Overlap {
      fragment1 :: Fragment
    , fragment2 :: Fragment
    , onLeft    :: Bool  -- True =>  fragment1--fragment2
                         -- False => fragment2--fragment1
    , olSeqLen  :: Int   -- maximum overlap length
    -- Can derive olSeq from above, and it is not needed for every
    -- fragment pair tested, just the pair with the largest olSeqLen.
    -- , olSeq :: Fragment  -- the overlapping sequence

    } deriving (Show)
            

parseLine :: String -> Fragments
parseLine = split ';' 

split :: Char -> String -> [String]
split cSplit s = (fst p) : (snd p)
    where p = foldr spl ("", []) s
          spl c (subStr, subStrs)
              | c == cSplit = ("", subStr:subStrs)
              | otherwise = (c:subStr, subStrs)


-- REFACTOR The problem with keeping a list of Fragments is that the
-- fragment overlaps must be recalculated each iteration. Try
-- memoization using a 1/2 diagonal matrix.

-- Remove the two fragments with the longest overlap. Add the fragment
-- created by merging the two overlapping fragments.
updateFragments :: Fragments -> Fragments
--updateFragments frags | trace (show frags) False = undefined
updateFragments (frag:[]) = [frag]
updateFragments frags = updateFragments frags'
    where
    frags' = if (olSeqLen o) == 0  -- no fragments overlap
             then longestFragLi
             else mergedFrag : deleteBothFrags

    longestFragLi = [maximumBy (comparing length) frags]

{-
    longestFragLi = [head . revSortFragsOnLen $ frags]

    -- longest to shortest
    revSortFragsOnLen = sortOn (negate . length)
-}

    deleteBothFrags = preFrag1 ++ preFrag2 ++ postFrag2
        where preFrag1 = takeWhile (/= fragment1 o) frags
              postFrag1 = (tail . dropWhile (/= fragment1 o)) frags
              preFrag2 = takeWhile (/= fragment2 o) postFrag1
              postFrag2 = (tail . dropWhile (/= fragment2 o)) postFrag1
        
    mergedFrag = if onLeft o
                 then leftMergedFrag
                 else rightMergedFrag
    leftMergedFrag =
        (fragment1 o) ++ drop (olSeqLen o) (fragment2 o)
    rightMergedFrag =
        (fragment2 o) ++ drop (olSeqLen o) (fragment1 o)

    o = longestOverlap frags
    

-- Given a list of fragments, find which pair of fragments has the longest
-- overlap and return that Overlap pair. O(n^2).
longestOverlap :: Fragments -> Overlap
longestOverlap frags = lOv where
--longestOverlap frags = head sos where
    -- olSeqLen field in Overlap datatype
    lOv = maximumBy (comparing olSeqLen) os
    --sos = sortOn (negate . olSeqLen) os
      
    -- (init . init . tails) ["a", "b", "c", "d"] =>
    -- [["a", "b", "c", "d"], ["b", "c", "d"], ["c", "d"]]

    -- All pairs are evaluted for maxOverlap:
    --   ab, ac, ad, bc, bd, cd
          
    -- The following 2 lines are like
    --   for (i=0; i<lenfrags-1; i++)
    --     for (j=i+1; j<lenfrags; j++)
    --       if (tmp = maxOverlap fragi fragj > longestOverlapSoFar)
    --         longestOverlapSoFar = tmp;
    -- except it's done without indices or loops.
    os = concatMap getPairs $ (init . init . tails) frags

    getPairs (frag':frags') = map (maxOverlap frag') frags'


{- Functions for overlaps of a pair of fragments. -}

-- About overlaps

-- When one considers overlaps, imagine matching the shorter fragment
-- above to the longer fragment below. The shorter fragment can match
-- either the left or the right side of the longer fragment.

-- Let f1 = "abcd", f2 = "abc", f3 = "bcd".

-- Pair f1,f2 shows a *left* overlap:
--   f2  abc
--   f1  abcd

-- Pair f1,f3 shows a *right* overlap:
--   f3   bcd
--   f1  abcd

-- In the following two cases, the algorithm has no way to choose
-- which fragment is <= the other, but the result is the same: the
-- overlap is bc.

-- If f2 <= f3, pair f2,f3 shows a left overlap, but no right overlap:
--   f2  bc
--   f3  bcd
               
-- If f3 <= f2, pair f3,f2 shows a right overlap, but no left overlap:
--   f3   bc
--   f2  abc

-- For a given pair of fragments ...
-- Return the maximal overlap data type Overlap.
-- Due to asymmetry, require frag1 <= frag2.

-- However, return frag1', frag2' in Overlap, because it results in a
-- simpler implementation of the mergedFrag var in the updateFragments
-- function. In other words, move the complexity to here in maxOverlap.
maxOverlap :: Fragment -> Fragment -> Overlap
maxOverlap frag1' frag2' = maxO where
    maxO = maximumBy (comparing  olSeqLen) [maxLeftO, maxRightO]
    --maxO = head (sortOn (negate . olSeqLen) [maxLeftO, maxRightO])

    -- If frag1' <= frag2', then frag1 <= frag2, and then the leftOL
    -- (onLeft field in Overlap) is True (noSwap) and the rightOL is
    -- False (swap). The logic is the same for frag1' >= frag2', but
    -- the result is opposite.
    maxLeftO = Overlap frag1' frag2' noSwap (leftOL frag1 frag2)
    maxRightO = Overlap frag1' frag2' swap (rightOL frag1 frag2)

    swap = frag1' /= frag1
    noSwap = not swap
                
    -- frag1 <= frag2
    (frag1, frag2) = sortFragsOnLen frag1' frag2'
    sortFragsOnLen f1' f2' = (f1, f2)
        where frags = sortOn length [f1', f2']
              f1 = head frags
              f2 = last frags

-- The longest left overlap length.
-- If f1 = "bab" and f2 = "abcd", then f1Tails = ["bab", "ab", "b"],
-- and overlapLens = [0, 2, 0], because "bab" does not left overlap
-- "abcd", "ab" does, and "b" does not.
-- f1 <= f2
leftOL :: Fragment -> Fragment -> Int
leftOL f1 f2 = longestLeftOL where
    longestLeftOL = firstNonZero overlapLens
    firstNonZero is = if null fnz
                      then 0
                      else head fnz
        where fnz = dropWhile (== 0) is

    overlapLens = map getOL f1Tails
    getOL subf1
        | isLeftO subf1 f2 = length subf1
        | otherwise = 0
    f1Tails = (init . tails) f1
    
-- The longest right overlap length.
-- Conceptually simple but expensive
-- f1 <= f2
rightOL :: Fragment -> Fragment -> Int
rightOL f1 f2 = leftOL (reverse f1) (reverse f2)
          
-- Does the longer fragment begin with the shorter fragment?
isLeftO :: Fragment -> Fragment -> Bool          
isLeftO f1 f2 = all (== True) $ zipWith (==) f1 f2





{-               
-- Does the longer fragment end with the shorter fragment?
isRightO :: Fragment -> Fragment -> Bool
-- If f1 << f2, quick & dirty approach converges on optimal.
isRightO f1 f2 = isLeftO (reverse f1) (reverse f2)  -- quick & dirty
-}


{- After finding the longest overlap
Remove frag(i) and frag(j) from the list, and add conjoined frag(i,j).
Put frag(i,j) towards end of list to process shorter fragments first. No.
-}
    
