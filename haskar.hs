{-# LANGUAGE GADTs #-}

import Data.List
import Data.Ratio
import qualified Data.Map as M
import Data.BitString

-- A basic type for a probability distribution and a constructor and instances for it
data Prob a = Prob [(a, Rational)]

makeProb :: [(a, Rational)] -> Prob a
makeProb items
    | (==1) . sum . (map snd) $ items = Prob items
    | otherwise = error "probabilities don't sum to 1"

instance (Show a) => Show (Prob a) where
    show (Prob items) = "Prob " ++ (show items)

instance (Eq a) => Eq (Prob a) where
    (Prob xs) == (Prob ys) = and $ map (flip elem xs) ys

instance Functor Prob where
    fmap func (Prob items) = Prob $ map (\(a,b) -> (func a, b)) items

sortProb :: (Ord a) => (Prob a) -> (Prob a)
sortProb (Prob items) = Prob $ sortBy (\ a b -> compare (fst a) (fst b)) items

widthOfInterval :: Interval -> Rational
widthOfInterval (a,b) = b - a

-- Arithmetic coding operations
type Interval = (Rational, Rational)
type IntervalMap a = M.Map a Interval
type UnitMap a = IntervalMap a

distToUnitMap :: (Ord a) => (Prob a) -> IntervalMap a
distToUnitMap (Prob []) = M.empty
distToUnitMap (Prob ((k,f):xs)) = M.union (M.singleton k (0, f)) (fmap (\(a,b) -> (a+f, b+f)) (distToUnitMap (Prob xs)))

-- Function assumes that the first item in the list is the first interval, the last item is the last interval, and they are adjacent
unionOfIntervals :: [Interval] -> Interval
unionOfIntervals intervals = (fst . head $ intervals, snd . last $ intervals)

unionOfMap :: (IntervalMap a) -> Interval
unionOfMap = unionOfIntervals . M.elems

-- Scales up or down an IntervalMap to cover a given interval
applyIntervalToMap :: (Ord a) => Interval -> IntervalMap a -> IntervalMap a
applyIntervalToMap new@(newStart, newEnd) m = fmap (\(a,b) -> (mapOldPoint a, mapOldPoint b)) m 
    where newWidth = widthOfInterval new
          oldInterval@(oldStart, oldEnd) = unionOfMap m
          oldWidth = widthOfInterval oldInterval
          mapOldPoint = (\a -> newStart + (newWidth / oldWidth*(a - oldStart)))

encodeToInterval :: (Ord a) => [a] -> IntervalMap a -> Interval
encodeToInterval (x:xs) m = encodeToInterval xs (applyIntervalToMap (unjust mi) m) 
    where mi = M.lookup x m
          unjust = (\ (Just a) -> a)
encodeToInterval [] m = unionOfIntervals . M.elems $ m

-- Encoding to a BitString
encodeToBitString :: Interval -> BitString
encodeToBitString = fromList . fst . (phaseOne [] (0 % 1, 1 % 1))

-- Phase one - while the target interval is contained in either half of the current interval, select that half and recurse, building a list of paths taken
phaseOne :: [Bool] -> Interval -> Interval -> ([Bool], Interval)
phaseOne currString currInterval target
    | (snd target) < midpoint = phaseOne (currString ++ [False]) (fst currInterval, midpoint) target
    | midpoint < (fst target) = phaseOne (currString ++ [True]) (midpoint, snd currInterval) target
    | otherwise = (currString, currInterval)
    where midpoint = ((fst currInterval) + (snd currInterval)) / 2

-- Phase two takes a target interval that straddles the midpoint of the current interval and expands the ''middle'' part of the current interval
--      until either of the middle quartile ranges is contained within the target interval.  It returns the number of expansions and a flag to indicate
--      which quartile was eventually chosen.


-- sample maps and testing code
p :: Prob Char
p = Prob [('a', 1 % 4), ('b', 1 % 4), ('c', 1 % 2)]

m :: IntervalMap Char
m = distToUnitMap p
