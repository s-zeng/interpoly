import Data.List

-- TODO
-- - Terminal features
--   - Help message
--   - Flag handling
--     - Precision flag
--     - Indexing at 0 or 1
-- - Remove extraneous 0s
-- - proper lagrange interpolation

main :: IO ()
main = getLine >>= (putStrLn . polyformat . powerfixed_solve . (map (read::String->Double)) . words) >> main

enumerate :: (Num a, Enum a) => [b] -> [(a, b)]
-- transforms [a,b,c] into [(0,a), (1,b), (2,c)]
enumerate = zip [0..]

is_constant :: (Ord a, Fractional a) => [a] -> Bool
is_constant []     = True
is_constant (x:xs) = all ((< 0.001) . abs . (subtract x)) xs

differences :: (Eq t, Num t, Num a) => t -> [a] -> [a]
-- returns the nth differences between elements of lst
differences n []  = [0]
differences 0 lst = lst
differences n lst = differences (n - 1) $ tail $ zipWith (-) lst $ 0:lst

degree :: (Ord a, Fractional a, Enum p, Num p) => [a] -> p
degree []  = 0
degree lst
  | is_constant lst = 0
  | otherwise       = succ . degree $ differences 1 lst

factorial :: Integer -> Integer
factorial = product . enumFromTo 1

solver :: (Enum a, Ord a, Fractional a) => [a] -> [a]
solver lst
  | deg == 0  = [a]
  | otherwise = [a] ++ solver [x - a*(n^deg) | (n, x) <- enumerate lst]
  where
      deg     = degree lst
      a       = (head $ differences deg lst) / fromIntegral (factorial deg)

powerfixed_solve :: (Enum a, Ord a, Fractional a) => [a] -> [a]
powerfixed_solve lst = solution ++ replicate pad 0
    where 
        solution = solver lst
        pad      = (degree lst) - length solution + 1

polyformat :: (RealFrac a, Show a, Eq a) => [a] -> String
polyformat = (intercalate " + ") . map poly . reverse . filter ((/= 0) . snd) . enumerate . reverse 
    where 
        poly (deg, val) = if deg /= 0 then make_integral val ++ "x^" ++ show deg else make_integral val
        make_integral x = if x == fromInteger (round x) then show $ round x else show x
