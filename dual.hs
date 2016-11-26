data Dual = Dual {real, imaginary :: Double}
  deriving (Show, Eq)

instance Num Dual where
  Dual a b + Dual c d = Dual (a+c) $ b+d
  Dual a b * Dual c d = Dual (a*c) $ a*d+b*c

  signum (Dual a b) = Dual (signum a) $ signum b
  abs (Dual a b) = Dual (abs a) $ b * signum a
  fromInteger a = Dual (fromInteger a) (fromInteger 0)

instance Fractional Dual where
  Dual a b / (Dual c d) = Dual (a / c) $ (c*d - a*d) / (c*c)
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


main = print a
  where
    x :: Integer
    x = 5
    y :: Dual
    y = Dual 2.0 4.5
    a :: Dual
    a = y*fromInteger x
