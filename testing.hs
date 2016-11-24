import Control.Applicative

type Name = String
data Employee = Employee { name    :: Name
                         , phone   :: String }
                deriving Show

names  = ["Joe", "Sara", "Mae"]
phones = ["555-5555", "123-456-7890", "555-4321"]
employees1 = Employee <$> names <*> phones
employees2 = getZipList $ Employee <$> ZipList names <*> ZipList phones

(.+) = liftA2 (+)    -- addition lifted to some Applicative context
(.*) = liftA2 (*)    -- same for multiplication
m1 = (Just 3 .+ Just 5) .* Just 8

addOneOrTwo :: Int -> [Int]
addOneOrTwo x = [x+1, x+2]

ex04 = [10,20,30] >>= addOneOrTwo

main = print ex04
