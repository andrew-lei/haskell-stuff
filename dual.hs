{-# LANGUAGE RankNTypes #-}

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

makevar :: Double -> Dual
makevar x = Dual x 1.0

deriv :: (Dual -> Dual) -> Double -> Double
deriv f = inf . f . makevar

legendre :: (Fractional a) => Int -> a -> a
legendre 0 _ = 1
legendre n x = helper 1 x n
  where
    helper _    acc2 1 = acc2
    helper acc1 acc2 n = helper acc2 rec (n-1)
      where
        n' = fromIntegral n
        rec = ((2*n' - 1) * x * acc2 - (n'-1) * acc1)/n'

hermite :: (Fractional a) => Int -> a -> a
hermite 0 _ = 1
hermite n x = helper 1 (2*x) n
  where
    helper _    acc2 1 = acc2
    helper acc1 acc2 n = helper acc2 rec (n-1)
      where
        n' = fromIntegral n
        rec = 2 * x * acc2 - 2 * (n' - 1) * acc1

test :: (forall a. Fractional a => (a -> a)) -> Double -> Double -> Double
test f x y = f'x * fy
  where
    f'x = (deriv f) x
    fy = f y

roots :: Double -> (forall a. Fractional a => (a -> a)) -> Int -> Double -> [Double]
roots tol f nroots lowbd = helper lowbd []
  where
    helper :: Double -> [Double] -> [Double]
    helper lowbd zeros
      | length zeros == nroots = zeros
      | otherwise = helper (newzero + tol) (newzero:zeros)
      where
        newzero :: Double
        newzero = helper' lowbd

        helper' :: Double -> Double
        helper' guess
          | (abs . real . f . makevar) guess < tol = guess
          --x_i = x_i - f(x_i) / (f'(x_i) - f(x_i) * sum_{j < i}(1 / (x_i - x_j)))
          | otherwise = helper' guess'
          where
            fguess :: Double
            fguess = f guess
            f'guess :: Double
            f'guess = (deriv f) guess
            transformation :: Double -> Double -> Double
            transformation acc zero = acc + 1 / (guess - zero)
            transformedSum :: [Double] -> Double
            transformedSum = foldl transformation 0
            guess' :: Double
            guess' = guess - fguess / (f'guess - fguess * transformedSum zeros)


main = print rs
  where
    x :: Integer
    x = 5
    y :: Dual
    y = Dual 2.0 4.5
    a :: Dual
    a = y*fromInteger x

    rootFinder = roots (10**(-12))
    lFinder = rootFinder (legendre 20) 20
    rs :: [Double]
    rs = lFinder (-1.0)
