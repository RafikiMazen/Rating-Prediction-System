member :: (Eq a) => a -> [a] -> Bool
member x [] = False
member x (y:ys) | x==y = True
                | otherwise = member x ys
				


dis (x:xs)= disH (x:xs) []

disH [] ys=[]
disH (x:xs) ys | member x ys= disH xs ys				
               | otherwise = (x:disH xs (x:ys)) 
			   

data U = U String deriving (Show,Eq) 
data I=I String deriving (Show,Eq)
data R= R Double|NoRating deriving (Show,Eq) 			   
fromRatingsToItems :: Eq a => [(b,a,c)] -> [a]
fromRatingsToItems []=[]
fromRatingsToItems ((y, x,r):xs)= dis (x:fromRatingsToItems xs)


fromRatingsToUsers :: Eq a => [(a,b,c)] -> [a]
fromRatingsToUsers []=[]
fromRatingsToUsers ((n,x,r):xs)= dis (n:fromRatingsToUsers xs)




hasRating :: (Eq a, Eq b) => a -> b -> [(a,b,c)] -> Bool
hasRating x y ((a,b,c):t) | x==a && y==b = True
                            |otherwise = (hasRating x y t)

hasRating x y [] =False


getRating :: (Eq a, Eq b) => a -> b -> [(a,b,c)] -> c
getRatingH :: (Eq a, Eq b) => a -> b -> [(a,b,c)] -> c

getRating x y l | hasRating x y l = getRatingH x y l 
				| otherwise = error "No given rating"
				
getRatingH x y ((a,b,z):t) | x==a && y==b = z
                           |otherwise= getRatingH x y t


						   

						   
formMatrixUser :: (Eq a, Eq b) => b -> [a] -> [(b,a,Double)] -> [R]
--formMatrixUser x (y:ys) t= ((R (getRating x y t)): formMatrixUser x ys t) 
--formMatrixUser x [] t=[] 
formMatrixUser x (y:ys) t|hasRating x y t=((R (getRating x y t)): formMatrixUser x ys t)
					|otherwise= (NoRating:formMatrixUser x ys t)
formMatrixUser x [] t=[]

formMatrix :: (Eq a, Eq b)=> [b] -> [a] -> [(b,a,Double)] -> [[R]]

formMatrix (x:xs) y t= formMatrixUser x y t : formMatrix xs y t
formMatrix [] y t= []

ratingexists :: Int -> [R] -> Int -> Bool	
								  
ratingexists n (x:xs) i |n==i = x/=NoRating
						| otherwise = ratingexists n xs (i+1)
						 
						 
numberRatingsGivenItem :: (Num b) => Int -> [[R]] -> b
numberRatingsGivenItem n [] =0						   
numberRatingsGivenItem 	n (x:xs) | ratingexists n x 0=1+(numberRatingsGivenItem n xs)
									  | otherwise = numberRatingsGivenItem n xs
									  
differenceRatings :: R -> R  -> Double									  
differenceRatings NoRating x=0
differenceRatings x NoRating=0
differenceRatings NoRating NoRating=0
differenceRatings (R x) (R y) =x-y


matrixPairs ::  Int -> [(Int,Int)]
matrixPairsH ::  Int-> Int->Int->[(Int,Int)]


matrixPairs n= matrixPairsH n 0 0

matrixPairsH n i j | i<n &&j<n =((i,j):matrixPairsH n i (j+1))
				   |i<n &&j>=n = matrixPairsH n (i+1) 0
				   |otherwise =[]
				   


dMatrix :: [[R]] -> [Double]
dMatrixH1 :: [[R]]->[(Int,Int)]->[Double]
dMatrixH2:: [[R]]->(Int,Int)->Double
dMatrix (x:xs)= dMatrixH1 (x:xs) (matrixPairs (length x))
dMatrixH1 h []=[]
dMatrixH1 h (m:ms) = ((dMatrixH2 h m):(dMatrixH1 h ms))
dMatrixH2 [] (i,j)=0 
dMatrixH2 [h] (i,j)=differenceRatings (h!!i) (h!!j)
dMatrixH2 (h:hs) (i,j)= (differenceRatings (h!!i) (h!!j))+dMatrixH2 hs (i,j)



freqMatrix ::  [[R]] -> [Int]
freqMatrixH1::[[R]]->[(Int,Int)]->[Int]
freqMatrixH2::[[R]]->(Int,Int)->Int
freqMatrixH3::R->R->Int
freqMatrix (x:xs)= freqMatrixH1 (x:xs) (matrixPairs (length x))
freqMatrixH1 h []=[]
freqMatrixH1 h (m:ms) = ((freqMatrixH2 h m):(freqMatrixH1 h ms))
freqMatrixH2 [] (i,j)=0 
freqMatrixH2 [h] (i,j)=freqMatrixH3 (h!!i) (h!!j)
freqMatrixH2 (h:hs) (i,j)= ((freqMatrixH3 (h!!i) (h!!j)) + freqMatrixH2 hs (i,j))
freqMatrixH3 x y | x/=NoRating && y/= NoRating = 1
				  |otherwise= 0

diffFreqMatrix :: [[R]] -> [Double]
diffFreqMatrixH1::[Double]->[Int]->[Double]
diffFreqMatrix r= diffFreqMatrixH1 (dMatrix r) (freqMatrix r)
diffFreqMatrixH1 [] []=[]
diffFreqMatrixH1 (x:xs) (y:ys)=  x /(fromIntegral y) :diffFreqMatrixH1 xs ys



userH l x = dis(fromRatingsToUsers l)!!x




itemH l x =dis(fromRatingsToItems l)!!x

diffinderH [] x y i j n =error"heheheh 2"
diffinderH (h:t) x y i j n | x==i && y==j = h
						  | i<n &&j<n = diffinderH t x y i (j+1) n 
						  | i<n &&j==n = diffinderH (h:t) x y (i+1) 0 n
						  | otherwise = error "Unexpected error"
sqrtH :: Float -> Int
sqrtH x = floor (sqrt x)			

--diffFreqMatrix (formMatrix (fromRatingsToUsers l) (fromRatingsToItems l) l)
helper1 list x ii = diffinder list ii x 
helper	l x ii= helper1 (diffFreqMatrix (formMatrix (fromRatingsToUsers l) (fromRatingsToItems l) l))  x 
diffinder l x y= diffinderH l x y 0 0 (sqrtH (fromIntegral (length l)))
predict l ui ii = if (hasRating (userH l ui) (itemH l ii) l) then (getRating (userH l ui) (itemH l ii) l) else predictHard l ui ii

predictHard l ui ii= averagex (finalListFind l ui ii)

averagex xs = sum xs / (fromIntegral(length xs))

avgDifFind l x y = diffinder ( diffFreqMatrix (formMatrix (fromRatingsToUsers l) (fromRatingsToItems l) l)) x y  

listOfAvg l x=listOfAvgH l x 0

listOfAvgH l x y | y<length((fromRatingsToItems l)) &&x/=y = avgDifFind l x y: listOfAvgH l x (y+1)
				 |y<length(fromRatingsToItems l)&& x==y = listOfAvgH l x (y+1)
				 |otherwise=[]

ratingsOfUser l ui= ratingsOfUserH l ui (userH l ui)
				 
ratingsOfUserH ((x,y,z):ls) ui a |x==a = (z:ratingsOfUserH ls ui a)
								|otherwise=ratingsOfUserH ls ui a
ratingsOfUserH [] ui a=[]

finalListFind l ui ii= finalListFindH (ratingsOfUser l ui) (listOfAvg l ii)

finalListFindH (u:us) (i:is)= (u+i):finalListFindH us is
finalListFindH [] []=[]
finalListFindH [] i= error "users ratings less than average"
finalListFindH u []=error	"avergae less than user ratings"
							