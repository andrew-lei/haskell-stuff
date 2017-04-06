data Dual = Dual {real, imaginary :: Double}
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

deriv :: (Dual -> Dual) -> Double -> Double
deriv f = imaginary . f . (flip Dual 1.0)

legendre :: (Fractional a) => Integer -> a -> a
legendre 0 _ = 1
legendre n x = helper x 1 x n
  where
    helper :: (Fractional a) => a -> a -> a -> Integer -> a
    helper _ _    acc2 1 = acc2
    helper x acc1 acc2 n = helper x acc2 rec (n-1)
      where 
        n' = fromInteger n
        rec = ((2*n' - 1) * x * acc2 - (n'-1) * acc1)/n' 

hermite :: (Fractional a) => Integer -> a -> a
hermite 0 _ = 1
hermite n x = helper x 1 (2*x) n
  where
    helper :: (Fractional a) => a -> a -> a -> Integer -> a
    helper _ _    acc2 1 = acc2
    helper x acc1 acc2 n = helper x acc2 rec (n-1)
      where
        n' = fromInteger n
        rec = 2 * x * acc2 - 2 * (n' - 1) * acc1


main = print a
  where
    x :: Integer
    x = 5
    y :: Dual
    y = Dual 2.0 4.5
    a :: Dual
    a = y*fromInteger x
