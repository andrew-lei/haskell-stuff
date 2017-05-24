import Data.List

-- Working on implementing the following:
-- https://bfi.uchicago.edu/sites/default/files/file_uploads/JEDC2014_JMMV.pdf

disjChebExts :: Int -> [Double]
disjChebExts i
  | i == 0 = [0]
  | i == 1 = [-1,1]
  | otherwise = map extremaFormula coprimeVals
    where
      n = 2^i
      -- filter out all k where k is not coprime with n'
      coprimeVals :: [Int]
      coprimeVals = filter (\k -> gcd k n == 1) [0..n]

      extremaFormula :: Int -> Double
      extremaFormula k = cos (k'/n' * pi)
        where
          k' = fromIntegral k
          n' = fromIntegral n

main = print $ disjChebExts 3
