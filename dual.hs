{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

import Data.List

data Dual = Dual {real, inf :: Double}
  deriving (Show, Eq)

instance Num Dual where
  Dual a b + Dual c d = Dual (a+c) $ b+d
  Dual a b * Dual c d = Dual (a*c) $ a*d+b*c

  signum (Dual a b) = Dual (signum a) $ signum b
  abs (Dual a b) = Dual (abs a) $ b * signum a
  fromInteger a = Dual (fromInteger a) (fromInteger 0)
  negate (Dual a b) = Dual (-a) (-b)

instance Fractional Dual where
  Dual a b / (Dual c d) = Dual (a / c) $ (c*b - a*d) / (c*c)
  fromRational a = Dual (fromRational a) (fromRational 0)

instance Floating Dual where
  pi = Dual pi 0.0

  sin (Dual a b) = Dual (sin a) $ b * cos a
  cos (Dual a b) = Dual (cos a) $ -b * sin a
  tan (Dual a b) = Dual (tan a) $ b / (x*x)
    where x = cos a

  asin (Dual a b) = Dual (asin a) $ b / sqrt(1 - a*a)
  acos (Dual a b) = Dual (acos a) $ -b / sqrt(1 - a*a)
  atan (Dual a b) = Dual (atan a) $ b / (1 + a*a)

  sinh (Dual a b) = Dual (sinh a) $ b * cosh a
  cosh (Dual a b) = Dual (cosh a) $ b * sinh a
  tanh (Dual a b) = Dual (tanh a) $ b / (x*x)
    where x = cosh a

  asinh (Dual a b) = Dual (asinh a) $ b / sqrt(a*a + 1)
  acosh (Dual a b) = Dual (acosh a) $ b / sqrt(a*a - 1)
  atanh (Dual a b) = Dual (atanh a) $ b / (1 - a*a)

  exp (Dual a b) = Dual (exp a) $ b * exp a
  log (Dual a b) = Dual (log a) $ b/a

-- makevar: Converts a Double to a variable with value of the Double
makevar :: Double -> Dual
makevar x = Dual x 1.0

-- deriv: Get derivative of function
deriv :: (Dual -> Dual) -> Double -> Double
deriv f = inf . f . makevar

-- legendre: Legendre polynomial of order n at x
-- Legendre order 0 is 1
legendre :: (Fractional a) => Int -> a -> a
legendre 0 _ = 1
legendre n x = helper 1 x n
  where
    -- Legendre is defined recursively in terms of
    -- lower order Legendre polynomials
    helper _    acc2 1 = acc2
    helper acc1 acc2 n = helper acc2 rec (n-1)
      where
        n' = fromIntegral n
        -- recursive definition of Legendre polynomial:
        -- n P_{n}(x) = (2n - 1) x P_{n-1}(x) - (n-1) P_{n-2}(x)
        rec = ((2*n' - 1) * x * acc2 - (n'-1) * acc1)/n'

-- hermite: Hermite polynomial of order n at x
-- hermite order 0 is 1
hermite :: (Fractional a) => Int -> a -> a
hermite 0 _ = 1
hermite n x = helper 1 (2*x) n
  where
    helper _    acc2 1 = acc2
    helper acc1 acc2 n = helper acc2 rec (n-1)
      where
        n' = fromIntegral n
        -- recursive definition of Hermite polynomial (physicists'):
        -- H_n(x) = 2 x H_{n-1}(x) - H'_{n-1}(x)
        -- H'_n(x) = 2 n H_{n-1}(x)
        -- H_n(x) =  2 x H_{n-1}(x) - 2 (n-1) H_{n-2}(x)
        rec = 2 * x * acc2 - 2 * (n' - 1) * acc1

-- roots: Multiple root finder using (a variant of) Newton's method
-- (forall a. Fractional a => (a -> a)) guarantees that function whose roots
-- we're looking for is guaranteed to have a derivative, since (Dual -> Dual)
-- is an instance (or something) of Fractional a => (a -> a)
-- This is based on the algorithm in this paper:
-- http://www.kurims.kyoto-u.ac.jp/EMIS/journals/AMI/2006/barrera.pdf
roots :: Double -> (forall a. Fractional a => (a -> a)) -> Int -> Double -> [Double]
roots tol f nroots lowbd = helper nroots lowbd []
  where
    -- helper: If number of zeros equals nroots, return the zeros
    -- Otherwise, find another zero, add it to the list, and begin
    -- searching for the next value, a little bit above the previous value
    helper :: Int -> Double -> [Double] -> [Double]
    helper 0 _     zeros = zeros
    helper n lowbd zeros = helper (n-1) (malu newzero) (newzero:zeros)
      where
        malu :: Double -> Double
        -- Move A Little Up
        -- Otherwise you're stuck and will get divide by zero
        malu x
          | x < 0 = x * (1 - tol)
          | x == 0 = tol
          | x > 0 = x * (1 + tol)

        newzero :: Double
        newzero = helper' lowbd

        -- helper': Implementation of Newton's Method (or, well, a variant)
        helper' :: Double -> Double
        helper' guess
          -- If |f(x)| < tol, it's good enough
          | (abs . f) guess < tol = guess
          -- Otherwise, iterate again
          | otherwise = helper' guess'
          where
            -- Traditionally, Newton's Method is x = x - f(x)/f'(x)
            -- What if you want to find more than one root?
            -- Factor out the previous roots you've found!
            -- g(x) = f(x) / Π_{j<i} (x - x_j)
            -- prod = Π_{j<i} (x - x_j)
            -- d/dx prod = Π_{j<i, j≠0} (x - x_j) + Π_{j<i, j≠1} (x - x_j) + ...
            --           = prod Σ_{j < i}(1 / (x - x_j))
            -- g'(x) = (f'(x)prod - f(x) d/dx prod) / prod^2
            --       = (f'(x) - f(x)Σ_{j < i}(1 / (x - x_j))) / prod
            -- So just find the roots of g!
            -- x_i = x_i - g(x_i) / g'(x_i)
            --     = x_i - f(x_i) / (f'(x_i) - f(x_i) Σ_{j < i}(1 / (x_i - x_j)))
            guess' :: Double
            guess' = guess - fguess / (f'guess - fguess * transformedSum zeros)

            fguess = f guess
            f'guess = (deriv f) guess
            transformation :: Double -> Double -> Double
            transformation acc zero = acc + 1 / (guess - zero)
            transformedSum :: [Double] -> Double
            transformedSum = foldl transformation 0

-- gaussQuadrature: Σ w/ω f(x)
gaussQuadrature :: [(Double, Double, Double)] -> Int -> (Double -> Double) -> Double
gaussQuadrature polyVals n f = sum $ map (apply f) polyVals
  where
    apply :: (Double -> Double) -> (Double, Double, Double) -> Double
    apply f (zero, omega, weight) = weight / omega * (f zero)

gaussLegendre :: Double -> Int -> (Double -> Double) -> (Double, Double) -> Double
gaussLegendre tol n = glIntegrate
  where
    zeros = roots tol (legendre n) n (-1)
    omegas = map (\x -> 1) zeros
    weights = map weightf zeros

    weightf x = 2 / ((1-x*x) * pn'x * pn'x)
      where
        pn'x = pn' x
        pn' = deriv $ legendre n

    polyVals = zip3 zeros omegas weights

    glIntegrate f (a,b) = gaussQuadrature polyVals n transformf
      where
        transformf x = (b-a)/2 * f ((b-a)/2 * x + (b+a)/2)

gaussHermite :: Double -> Int -> (Double -> Double) -> Double
gaussHermite tol n = ghIntegrate
  where
    hermiteLB :: Int -> Double
    hermiteLB n = - (fromIntegral n - 1) * sqrt (2 / (fromIntegral n + 2))
    zeros = roots tol (hermite n) n (hermiteLB n)
    omegas = map (\x -> exp (-x*x)) zeros
    weights = map weightf zeros

    -- w_i = 2^{n-1} n! sqrt(pi) / (n^2 H_{n-1}(x_i)^2)
    -- H_{n-1}(x) = H'_n(x) / (2n)
    weightf x = 2 ** (fromIntegral n+1) * fromIntegral (fact 1 n) * sqrt pi / (hn'x * hn'x)
      where
        fact acc 0 = acc
        fact acc n = fact (n*acc) (n-1)

        hn'x = deriv (hermite n) $ x

    polyVals = zip3 zeros omegas weights

    ghIntegrate :: (Double -> Double) -> Double
    ghIntegrate f = gaussQuadrature polyVals n f


gaussian :: Double -> Double
gaussian x = 1 / (sqrt (2*pi)) * exp (-x*x/2)

main = do
    --print $ rs
    --print $ minimum diffs
    print $ maximum zs
    --print $ ((ghtn (\x -> exp (-x*x)))^2 - pi)/pi
    --print $ gltn gaussian (-1,1)
    --print $ gltn gaussian (-1.96,1.96)
    --print $ gltn gaussian (-2,2)
    --print $ gltn gaussian (-3,3)
      where
        x :: Integer
        x = 5
        y :: Dual
        y = Dual 2.0 4.5
        a :: Dual
        a = y*fromInteger x

        n = 383
        ln = legendre n
        hn = hermite n

        tol = (10**(-12))

        rootFinder = roots tol
        lFinder = rootFinder ln n
        hFinder = rootFinder hn n
        hermiteLB n = - (n-1) * sqrt (2 / (n+2))

        rs :: [Double]
        rs = lFinder (-1.0)
        --rs = hFinder (hermiteLB n)

        zs :: [Double]
        zs = map ln rs

        rs' = sort rs
        diffs = zipWith (-) (tail rs') rs'

        gltn = gaussLegendre tol n
        ghtn = gaussHermite tol n
