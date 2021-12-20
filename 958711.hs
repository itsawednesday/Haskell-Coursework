
-- MATHFUN
--  958711
--
import Text.Printf
import Data.Char
import  Data.List (intercalate)
import Data.Ord
--Types 
type Location = String
type Degrees = (Float, Float)
type Degrees1 = Float
type Degrees2 = Float
type Populations = [Int]

type City =  (Location, Degrees1, Degrees2, Populations)


--MY HELPER FUNCTIONS
-- ===============================================================
--Takes location and gives population figures of a city
getPop :: Location -> [City] -> Populations 
getPop  location [] = []
getPop location ((loc, de1, de2, pop):xs)
    | location == loc = pop 
    | otherwise  = getPop location xs  --getPop "Athens" [("Amsterdam" ,  52 ,  5  ,  [1158, 1149, 1140, 1132]),("Athens"   ,    38 , 23  ,  [3153, 3153, 3154, 3156])]

-- Gives population figure from index
getYear :: Populations -> Int -> String
getYear (x:xs) yr
    | yr == 0 = printf "%.3f m" (fromIntegral x / 1000 :: Float)
    | otherwise = getYear xs (yr - 1) --getYear [1158, 1149, 1140, 1132] 2

-- Give just the city name from the list
getName :: City -> Location
getName (name, _, _, _) = name

--  FUNCTIONAL CODE
-- ===================================================================

-- Returns all the city names available in the database
-- getCities unlines . map(\(name, de1, de2, pop ) -> name)
getCities :: City -> String
getCities (loc, de1, de2, pop) = loc
  

-- Returns population of a city as a string from specified city name and number
getPopulationString :: [City] -> Location -> Int -> String
getPopulationString [] _ _ = []
getPopulationString cities loc yr = getYear (getPop loc cities) yr 



 --Returns City values in five columns
citiesToString :: [City] -> String
citiesToString [] =  "" 
citiesToString ((loc, de1, de2, pop): xs) = "\n " ++  (printf "%-11s" loc) ++ " " ++ show de1 ++ " " ++ printf "%-5s" (show de2) ++ " " ++ show pop  ++ "\n"  ++  (citiesToString  xs)


-- Update population value with a new list so this year's population value is new. There should be new value for each city
updatePopulation :: [City] -> Populations -> [City]
updatePopulation [] _ = []
updatePopulation (n:ns) (x:xs) = let (loc, de1, de2, pop) = n in (loc, de1, de2, x:pop):updatePopulation ns xs 


--Add new city preserving its alphabetical ordering. New city must have same population length. 
addNewCity :: [City] -> Location -> Degrees1 -> Degrees2 -> Populations -> [City]
addNewCity [] name de1 de2 pop = [(name, de1, de2, pop)]
addNewCity (city:cities) name de1 de2 pop 

    | getName city >= name = (name, de1, de2, pop) :cities    --insert the new city name after
    | otherwise = city : addNewCity cities name de1 de2 pop  -- recursion to insert new city into cities


-- Return a list of annual percentage population growth figures cities
populationGrowth :: [City] -> Location -> [Float]
populationGrowth [] _ = []
populationGrowth (city:cities) name 
  | name == (getName city) =  (percentIncrease (getPop name (city:cities) ))-- call percent function to return percentage growth of that city
  
  | otherwise = populationGrowth cities name 

percentIncrease :: [Int] -> [Float]
percentIncrease  (x:y:xs) =  percent x y : percentIncrease (y:xs) 
percentIncrease n = []

percent :: Int -> Int -> Float
percent w z =  (100 * (( a- b) / a )   )
  where a =   fromIntegral w :: Float
        b =   fromIntegral z :: Float



-- Return closest city with population bigger than 2m
pythagoreanTheorem :: Float -> Float -> Float
pythagoreanTheorem x y = sqrt (x * x + y * y)  

getDistance :: (Degrees1, Degrees2) -> (Degrees1, Degrees2) -> Float
getDistance (x, y) (x1, y1) = pythagoreanTheorem (x - x1) (y - y1)


-- findNearestCity :: [City] -> Int -> Degrees1 -> Degrees2 -> City
-- findNearestCity [] _ _ _ = []
-- closestCity city num de1 de2  = filter (\x -> (name x >= num )) city
-- findNearestCity city num de1 de2 = head (sortBy 



-- --
-- --  Demo
-- --

demo :: Int -> IO ()
--demo 1 = putStrLn (testData ) 
demo 2 = putStrLn  (getPopulationString testData "Madrid" 2 )
demo 3 = putStrLn  (citiesToString testData)

demo 4 =  putStrLn (citiesToString (updatePopulation testData [1200,3200,3600,2100,1800,9500,6700,11100,4300,1300,2000,1800]))
demo 5 = putStrLn (citiesToString (addNewCity testData "Prague" 50 14 [1312, 1306, 1299, 1292]))

demo 6 = print   (populationGrowth  testData "London" ) 

--demo 7 = -- output the nearest city to location (54N ,6E) with 
--          -- a population above 2m people
-- demo 8 = -- output the population map
demo _ = putStrLn "Error Choose 2-6"


-- --
-- -- Screen Utilities (use these to do the population map)
-- --

type ScreenPosition = (Int,Int)

-- Clears the screen
clearScreen :: IO ()
clearScreen = putStr "\ESC[2J"

-- Moves to a position on the screen
goTo :: ScreenPosition -> IO ()
goTo (x, y) = putStr ("\ESC[" ++ show y ++ ";" ++ show x ++ "H")

-- Writes a string at a position on the screen
writeAt :: ScreenPosition -> String -> IO ()
writeAt position text = do
    goTo position
    putStr text
 

--
-- Your population map code goes here
--

characterWidth :: Double
characterWidth = 80

characterHeight :: Double
characterHeight = 50



--UI
main :: IO ()
main = do
  loadFile <- readFile "cities.txt"
  putStrLn "City Data Loaded:\n"
  userInterface  (read loadFile :: [City])

userInterface :: [City] -> IO ()
userInterface list = do

  putStrLn "1. Display all cities"
  putStrLn "2. Display population of a city."
  putStrLn "3. Display all cities in neatly formatted columns."
  putStrLn "4. Insert new population figure for a city."
  putStrLn "5. Add a new city."
  putStrLn "6. Display populationgrowth of a city."
  putStrLn "7. Return the city closest to the input number."
  putStrLn "8. Exit"

  putStr "Input: "

  option <- getLine
  chooseOption option list

chooseOption :: String -> [City] -> IO ()

-- -- chooseOption "1" list = do
-- --   putStrLn (getCities  list)
-- --   userInterface list

chooseOption "2" list = do
  putStrLn (getPopulationString  list "Madrid"  2)
        
  userInterface list 

chooseOption "3" list = do
  putStrLn (citiesToString  list)
  userInterface list

chooseOption "4" list = do
  putStrLn  (citiesToString (updatePopulation list [1200,3200,3600,2100,1800,9500,6700,11100,4300,1300,2000,1800]))
  userInterface list

chooseOption "5" list = do
  putStrLn (citiesToString (addNewCity list "Prague" 50 14 [1312, 1306, 1299, 1292]))
  userInterface list


chooseOption "6" list = do
  putStrLn (populationGrowth  list "London" )
  userInterface list




type Database = [City]
testData :: [City]
testData = [("Amsterdam" ,  52 ,  5  ,  [1158, 1149, 1140, 1132]),
    ("Athens"   ,    38 , 23  ,  [3153, 3153, 3154, 3156]),
    ("Berlin"  ,     53,  13  ,  [3567, 3562, 3557, 3552]),
    ("Brussels"   ,  51  , 4  ,  [2096, 2081, 2065, 2050]),
    ("Bucharest"  ,  44 , 26  ,  [1794, 1803, 1812, 1821]),
    ("London"    ,   52 ,  0  ,  [9426, 9304, 9177, 9046]),
    ("Madrid"  ,     40  , 4  ,  [6669, 6618, 6559, 6497]),
    ("Paris"  ,      49 ,  2  ,  [11079, 11017, 10958, 10901]),
    ("Rome"     ,    42 , 13  ,  [4278, 4257, 4234, 4210]),
    ("Sofia"   ,     43 , 23  ,  [1284, 1281, 1277, 1272]),
    ("Vienna"    ,   48  ,16  ,  [1945, 1930, 1915, 1901]),
    ("Warsaw"   ,    52 , 21  ,  [1790, 1783, 1776, 1768])]
    