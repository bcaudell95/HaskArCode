{-# LANGUAGE GADTs #-}

import Data.List
import Data.Ratio
import qualified Data.Map as M
import qualified Data.BitString as BS
import qualified Data.Bits as B

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

-- Now we can generalize this type to a general probability model that depends on past inputs
-- In this model format, the likelihood of the next symbol is calculated as a funciton of the previous string
type Model a = ([a] -> Prob a)

basicModel :: Prob a -> Model a
basicModel = const

-- Arithmetic coding operations
-- Main encoding function
arithmeticEncode :: (Ord a) => Model a -> [a] -> BS.BitString
arithmeticEncode model str = encodeToBitString $ encodeWithModel [] str model (0 % 1, 1 % 1) 

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

encodeWithModel :: (Ord a) => [a] -> [a] -> Model a -> Interval -> Interval
encodeWithModel _ [] _ curr = curr
encodeWithModel prev (this:rest) model currInterval = encodeWithModel (prev ++ [this]) rest model newInterval
    where prob = model prev
          newInterval = (\(Just a) -> a) $ M.lookup this $ applyIntervalToMap currInterval $ distToUnitMap prob

-- Encoding to a BitString
-- TODO: turn this to point-free, likely using Applicative magic
encodeToBitString :: Interval -> BS.BitString
encodeToBitString target = BS.fromList $ (fst phaseOneResult) ++ phaseTwoResult
    where phaseOneResult = phaseOne [] (0 % 1, 1 % 1) target
          phaseTwoResult = phaseTwoOutput $ phaseTwo (snd phaseOneResult) target

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
data QuartileChoice = Q2 | Q3 deriving Show
phaseTwo :: Interval -> Interval -> (QuartileChoice, Int)
phaseTwo currInterval target
    | (fst target) <= q1 = (Q2, 0)
    | q3 <= (snd target) = (Q3, 0)
    | otherwise = fmap (+1) $ phaseTwo (q1, q3) target
    where q1 = ((widthOfInterval currInterval) / 4) + (fst currInterval)
          q3 = q1 + ((widthOfInterval currInterval) / 2)

phaseTwoOutput :: (QuartileChoice, Int) -> [Bool]
phaseTwoOutput (Q2, count) = [False] ++ (take (count+1) $ repeat True)
phaseTwoOutput (Q3, count) = [True] ++ (take (count+1) $ repeat False)

-- Decoder
boolToInt :: Bool -> Integer
boolToInt True = 1
boolToInt False = 0

bitStringToInteger :: BS.BitString -> Integer
bitStringToInteger bs = sum $ zipWith (\bit pow -> (boolToInt bit) * (B.shiftL 1 pow)) (reverse . BS.toList $ bs) [0..] 

bitStringToRational :: BS.BitString -> Rational
bitStringToRational bs = (bitStringToInteger bs % B.shiftL 1 (fromIntegral (BS.length bs)))

decodeBitStringToLength :: (Ord a) => BS.BitString -> Model a -> Int -> [a]
decodeBitStringToLength bs model len = decodeStringOfLength model (bitStringToRational bs) len

decodeStringOfLength :: (Ord a) => Model a -> Rational -> Int -> [a]
decodeStringOfLength = decodeStringInInterval [] (0 % 1, 1 % 1)

decodeStringInInterval :: (Ord a) => [a] -> Interval -> Model a -> Rational -> Int -> [a]
decodeStringInInterval prev currInt model target counter
    | counter == 0 = prev
    | otherwise = decodeStringInInterval (prev ++ [sym]) nextInterval model target (counter - 1)
        where (sym, nextInterval) = decodeSymbol symbolMap currInt target
              symbolMap = applyIntervalToMap currInt $ distToUnitMap $ model prev 

decodeSymbol :: (Ord a) => UnitMap a -> Interval -> Rational -> (a, Interval)
decodeSymbol symbolMap currInterval target = output
    where items = M.toList . (applyIntervalToMap currInterval) $ symbolMap
          output@(sym, newInterval) = (\(Just a) -> a) . (find $ (inInterval target) . snd) $ items  

inInterval :: Rational -> Interval -> Bool
inInterval target (a,b) = (a <= target) && (target <= b)

-- sample maps and testing code
p :: Prob Char
p = Prob [('a', 3 % 5), ('b', 1 % 5), ('c', 1 % 10), ('d', 1 % 10)]

m :: IntervalMap Char
m = distToUnitMap p

-- More complex probability model used in the report
p2 :: Model Char
p2 [] = Prob $ map (\a -> (a, 1 % 5)) "abcde"
p2 cs = sortProb . Prob $ (++) [(c, 1 % 2)] $ map (\a -> (a, 1 % 8)) rest
    where c = last cs
          rest = delete c "abcde" 

