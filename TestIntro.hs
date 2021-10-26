module TestIntro where
import Intro
import Data.Ratio

data Result a = Good | Bad [a] deriving (Show) 

toResult :: Result a -> (a -> Bool) -> a -> Result a
toResult Good bl val      = case (bl val) of True -> Good 
                                             False -> Bad [val]  
toResult (Bad xs) bl val  = case (bl val) of True -> (Bad xs)
                                             False ->Bad (val:xs)

test :: (a -> Bool) -> [a] -> Result a 
test check cases  = foldr (\x y -> toResult y check x) Good cases  

powerlst = map PowerBasis [[1,0,0], [1%3,3%4,4%5],[1,2,3,4]] 
risinglst =  map RisingBasis [[0,0,1], [1%44,3%7,4%9],[3,8,9,10]]

testPowerid :: Result PowerBasis 
testPowerid = test check powerlst where
                check x = x == (fromRisingBasisToPowerBasis . fromPowerBasisToRisingBasis) x

testRisingid :: Result RisingBasis
testRisingid = test check risinglst where 
                 check x = x == (fromPowerBasisToRisingBasis . fromRisingBasisToPowerBasis) x


cartProd xs ys = [(x,y) | x <- xs , y <- ys] 
sumIndices = [(0,0),(0,20),(77,89),(1001,2000)]

testPowerSum :: Result (PowerBasis,(Integer,Integer))
testPowerSum = test check (cartProd powerlst sumIndices) where 
                check (p,(l,r))= summation p l r == (sum $ map (eval p) [l..r])  
                
testRisingSum :: Result (RisingBasis, (Integer,Integer))
testRisingSum = test check (cartProd risinglst sumIndices) where 
                check (rb,(l,r))= summation rb l r == (sum $ map (eval rb) [l..r])  

lsts = [[1,0,0],[0,1,0],[0,0,1],[0,2,0]] 
lstSumIndexes = [(0,100),(-5,5),(-44,43),(-1,-1)] 
res  = [101,0,5684,-2]
cmb = \x,y -> (x,y) 


