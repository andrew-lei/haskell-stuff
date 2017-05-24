data Symbol = S | K | I | Symbol String
  deriving Eq

instance Show Symbol where
  show S = "S"
  show K = "K"
  show I = "I"
  show (Symbol x) = x

data Expression = El Symbol | Ex [Expression]
  deriving Eq

instance Show Expression where
  show (El x) = show x
  show (Ex xs) = '(' : ((concatMap show) xs) ++ ")"

simplify :: Expression -> Expression
simplify x@(El _)               = x
simplify (Ex [x@(El _)])        = x

simplify (Ex (El I:rest))       = Ex rest
simplify (Ex (El K:x:y:rest))   = Ex (x:rest)
simplify (Ex (El S:x:y:z:rest)) = Ex (x:z:Ex [y,z]:rest)

simplify (Ex (x@(El _):rest))   = Ex (x:map simplify rest)
simplify (Ex ((Ex x):rest))     = Ex (x ++ rest)

fullSimplify :: Expression -> Expression
fullSimplify x = helper x (simplify x)
  where helper a b
         | a == b    = a
         | otherwise = helper b (simplify b)

ignoreForst = Ex [El S, El K]
rev = Ex [El S, Ex [El K, Ex [El S, El I]], El K]
iota = Ex [El S, El S, Ex [El K, Ex [El K, El K]], Ex [El S, El I, Ex [El K, El S]]]

t = El K
f = Ex [El S, El K]
or' = t
and' = f

main = do
  print $ fullSimplify $ Ex [El S, El K, El (Symbol "x"), El (Symbol "y")]
  print $ fullSimplify $ Ex [rev, El (Symbol "x"), El (Symbol "y")]
  print $ fullSimplify $ Ex [iota, El (Symbol "x")]
  print $ fullSimplify $ Ex [iota, iota, El (Symbol "x")]
  print $ fullSimplify $ Ex [iota, Ex [iota, Ex [iota, iota]]]
  print $ fullSimplify $ Ex [iota, Ex [iota, Ex [iota, Ex [iota, iota]]]]
  putStrLn "Booleans"
  print $ fullSimplify $ Ex [t,or',t]
  print $ fullSimplify $ Ex [t,or',f]
  print $ fullSimplify $ Ex [f,or',t]
  print $ fullSimplify $ Ex [f,or',f]
  print $ fullSimplify $ Ex [t,t,and']
  print $ fullSimplify $ Ex [t,f,and']
  print $ fullSimplify $ Ex [f,t,and']
  print $ fullSimplify $ Ex [f,f,and']
