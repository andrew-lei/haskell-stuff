import Prelude hiding (List)

data List a = Empty
  | Cons (Singleton a) (List a)
  | Range (Singleton a) (Singleton a)
  | Map (String, (a -> a)) (List a)
  | Filter (String, (a -> Bool)) (List a)
  | Tail Int (List a)
  | ListError String

data Singleton a = El a
  | Op (String, (a -> a -> a)) (Singleton a) (Singleton a)
  | Foldl (String, (a -> a -> a)) (Singleton a) (List a)
  | Foldr (String, (a -> a -> a)) (Singleton a) (List a)
  | Head (List a)
  | Error String

data Expression a = Singleton (Singleton a)
  | List (List a)

instance Show a => Show (List a) where
  show Empty             = "[]"
  show (Cons x xs)       = show x ++ ':':show xs
  show (Range x y)       = '[':show x ++ "..." ++ show y ++ "]"
  show (Map (f,_) xs)    = "map (" ++ f ++ ") (" ++ show xs ++ ")"
  show (Filter (f,_) xs) = "filter (" ++ f ++ ") (" ++ show xs ++ ")"
  show (Tail n xs)       = "tail^" ++ show n ++ " (" ++ show xs ++ ")"
  show (ListError e)     = e

instance Show a => Show (Singleton a) where
  show (El x)               = show x
  show (Op (op,_) x y)      = show x ++ op ++ '(':show y ++ ")"
  show (Foldl (op, _) x xs) = "foldl (" ++
                              op ++ ") (" ++
                              show x ++ ") (" ++
                              show xs ++ ")"
  show (Foldr (op, _) x xs) = "foldr (" ++
                              op ++ ") (" ++
                              show x ++ ") (" ++
                              show xs ++ ")"
  show (Head x)             = "head (" ++ show x ++ ")"
  show (Error e)            = e

instance Show a => Show (Expression a) where
  show (Singleton x) = show x
  show (List x)      = show x

eval' :: (Num a, Ord a) => Singleton a -> Singleton a
eval' (Op op@(_, op') (El x) (El y)) = El (x `op'` y)
eval' (Op op x@(El _) y)             = Op op x (eval' y)
eval' (Op op x y)                    = Op op (eval' x) y

eval' (Foldl op acc Empty)       = acc
eval' (Foldl op acc (Cons x xs)) = Foldl op (Op op acc x) xs
eval' (Foldl op acc xs)          = Foldl op acc (eval'' xs)

eval' (Foldr op acc Empty)       = acc
eval' (Foldr op acc (Cons x xs)) = Op op x (Foldr op acc xs)
eval' (Foldr op acc xs)          = Foldr op acc (eval'' xs)

eval' (Head (Cons x xs))         = x
eval' (Head Empty)               = Error "Empty List"
eval' (Head xs)                  = Head (eval'' xs)

eval' other = other

eval'' :: (Num a, Ord a) => List a -> List a
eval'' (Cons x@(El _) xs) = Cons x (eval'' xs)
eval'' (Cons x xs)        = Cons (eval' x) xs

eval'' (Range (El x) (El y))
  | x < y                 = Cons (El x) $ Range (El (x+1)) (El y)
  | x == y                = Cons (El x) Empty
  | otherwise             = Empty
eval'' (Range x@(El _) y) = Range x (eval' y)
eval'' (Range x y)        = Range (eval' x) y

eval'' (Map _ Empty)                     = Empty
eval'' (Map f@(_,f') (Cons (El x) xs)) = Cons (El (f' x)) (Map f xs)
eval'' (Map f xs)                     = (Map f (eval'' xs))

eval'' (Filter _ Empty)                     = Empty
eval'' (Filter f@(_,f') (Cons (El x) xs))
 | f' x              = Cons (El x) (Filter f xs)
 | otherwise         = Filter f xs
eval'' (Filter f xs) = (Filter f (eval'' xs))

eval'' (Tail 0 xs)          = xs
eval'' (Tail _ Empty)       = ListError "Empty List"
eval'' (Tail n (Cons x xs)) = Tail (n-1) xs
eval'' (Tail n xs)          = Tail n (eval'' xs)

eval'' Empty = Empty
eval'' e@(ListError _) = e

eval :: (Num a, Ord a) => Expression a -> Expression a
eval (Singleton x) = Singleton (eval' x)
eval (List x)      = List (eval'' x)

loop :: (Show a, Num a, Ord a) => Expression a -> Int -> IO ()
loop expr n
  | n > 0     = do
      print expr
      loop (eval expr) (n-1)
  | otherwise = return ()


main = do
  loop (Singleton (Head (Tail 3 (Filter ("\\x -> x > 5", \x -> x > 5) (Map ("\\x -> x*x", \x -> x*x) (Range (El 1) (El 10))))))) 33
