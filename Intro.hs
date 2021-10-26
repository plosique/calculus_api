module Intro where
import Data.Array
import Data.Ratio


--break out RationalListFunction to multiple typeclasses
class  RationalListFunction a where
    eval  ::  a -> Rational -> Rational
    coeff ::  a -> [Rational] 
    antiDifferenceFunction :: a -> a
    summation :: a -> Integer -> Integer -> Rational
    summation p l r = f ratr - f (ratl-1) where
                      f = eval (antiDifferenceFunction p)
                      ratr = toRational r
                      ratl = toRational l

data PowerBasis = PowerBasis [Rational] deriving (Show, Eq) 
data RisingBasis = RisingBasis [Rational] deriving (Show, Eq)

instance RationalListFunction PowerBasis where
    coeff (PowerBasis lst) = lst

    eval p x = sum $ zipWith (*) (scanl (*) 1 [x | _ <- [1 .. ]]) (coeff p)      

    antiDifferenceFunction p  = (fromRisingBasisToPowerBasis . shiftRightRising . fromPowerBasisToRisingBasis) p

    summation p l r = summation (fromPowerBasisToRisingBasis p) l r --more efficient than default summmation

instance RationalListFunction RisingBasis where
    coeff (RisingBasis lst) = lst

    eval r x = sum $ zipWith (*) (scanl (*) 1 [x+i | i <- [0 .. ]]) (coeff r)      

    antiDifferenceFunction r = shiftRightRising r

fact = (scanl (*) (toRational 1) [1 .. ]) 
divideByFact xs = zipWith (\x y -> x/y) xs fact


difference :: (Num a, Num b) => (a -> b) -> (a -> b) 
difference f = \x -> f x - f (x-1)   

toRisingBasis :: (Rational -> Rational) -> Int -> RisingBasis 
toRisingBasis f n = RisingBasis lst where
                    lst = divideByFact $ map (\h -> h $ toRational 0) $  zipWith ($)  (scanl (.) id [difference | _ <- [1 .. n]]) [f | _ <- [1 .. n]]

fromPowerBasisToRisingBasis :: PowerBasis -> RisingBasis
fromPowerBasisToRisingBasis pb = toRisingBasis (\x -> eval pb x) (length $ coeff pb)  



buildStirling :: Integer -> Array (Integer, Integer) Integer
buildStirling n = arr where
                  arr = array ((0,0),(n,n)) (baseCase ++ [((i,j),f i j) | i <- [1 .. n], j<- [0 .. n]]) 
                  baseCase = ((0,0),1) : [((0,k),0) | k <- [1 .. n]]
                  f :: Integer -> Integer -> Integer
                  f i j | j==0       = 0 
                        | otherwise  = (j-1)*arr!(i,j-1) + arr!(i-1,j-1) 

stirling = fmap toRational (buildStirling 1000)

fromRisingBasisToPowerBasis :: RisingBasis -> PowerBasis
fromRisingBasisToPowerBasis rf  = PowerBasis lst where 
                            lst = [solveCoeff i | i <- [0 ..(n-1)]] 
                            solveCoeff i = (sum $ zipWith (*) [stirling!(i,j) | j <- [0 .. (n-1)]] (coeff rf)) 
                            n = toInteger $ length $ coeff rf

shiftRightRising :: RisingBasis -> RisingBasis 
shiftRightRising rb = RisingBasis lst where
                       lst = 0 : zipWith (*) [1 % i | i <- [1 ..]] (coeff rb)   

antiDifference :: [Rational] -> [Rational]
antiDifference cs = coeff $ antiDifferenceFunction (PowerBasis cs) 

sum :: [Rational] -> Integer -> Integer ->  Rational 
sum cs l r = summation (PowerBasis cs) l r



  
