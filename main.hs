import Data.List

main :: IO ()
main = do
    values <- getLine
    putStrLn . polyformat . powerfixed_solve . (map (read::String->Double)) . words $ values

enumerate :: (Num a, Enum a) => [b] -> [(a, b)]
-- transofmrs [a,b,c] into [(0,a), (1,b), (2,c)]
enumerate = zip [0..]

is_constant :: (Ord a, Fractional a) => [a] -> Bool
is_constant [] = True
is_constant (x:xs) = all ((< 0.001) . abs . (subtract x)) xs

differences :: (Eq t, Num t, Num a) => t -> [a] -> [a]
-- returns the nth differences between elements of lst
differences 0 lst = if null lst then [0] else lst
differences n []  = [0]
differences n lst = differences (n - 1) $ tail $ zipWith (-) lst $ 0:lst

degree :: (Ord a, Fractional a, Enum p, Num p) => [a] -> p
-- degree :: (Enum p, Eq a, Num p, Num a) => [a] -> p
degree []  = 0
degree lst
  | is_constant lst = 0
  | otherwise       = succ . degree $ differences 1 lst

factorial :: Int -> Integer
factorial = product . flip take [1..]

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
        pad = (degree lst) - length solution + 1

polyformat :: Show a => [a] -> String
polyformat = (intercalate " + ") . (map poly) . reverse . enumerate . reverse . (map show)
    where 
        poly x = if (fst x) /= 0 then snd x ++ "x^" ++ show (fst x) else snd x
