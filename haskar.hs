{-# LANGUAGE GADTs #-}

import Data.List
import Data.Ratio
import qualified Data.Map as M

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

-- Arithmetic coding operations
type Interval = (Rational, Rational)
type IntervalMap a = M.Map a Interval
type UnitMap a = IntervalMap a

distToUnitMap :: (Ord a) => (Prob a) -> IntervalMap a
distToUnitMap (Prob []) = M.empty
distToUnitMap (Prob ((k,f):xs)) = M.union (M.singleton k (0, f)) (fmap (\(a,b) -> (a+f, b+f)) (distToUnitMap (Prob xs)))

applyIntervalToMap :: (Ord a) => Interval -> IntervalMap a -> IntervalMap a
applyIntervalToMap (start, end) m = fmap (\(a,b) -> (start + (width*a), start + (width*b))) m 
    where width = (end - start) / ((\ (a,b) -> b - a) . unionOfIntervals . M.elems $ m)

-- Function assumes that the first item in the list is the first interval, the last item is the last interval, and they are adjacent
unionOfIntervals :: [Interval] -> Interval
unionOfIntervals intervals = (fst . head $ intervals, snd . last $ intervals)

encodeToInterval :: (Ord a) => [a] -> IntervalMap a -> Interval
encodeToInterval (x:xs) m = encodeToInterval xs (applyIntervalToMap (unjust mi) m) 
    where mi = M.lookup x m
          unjust = (\ (Just a) -> a)
encodeToInterval [] m = unionOfIntervals . M.elems $ m
