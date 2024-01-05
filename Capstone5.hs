-- code example from Lesson 18 - Data.Map

import qualified Data.Map as Map
  
data Organ = Heart | Brain | Kidney | Spleen deriving (Show, Eq)
  
organs :: [Organ]
organs = [Heart,Heart,Brain,Spleen,Spleen,Kidney]
  
ids :: [Int]
ids = [2,7,13,14,21,24]
  
organPairs :: [(Int,Organ)]
organPairs = zip ids organs
  
organCatalog :: Map.Map Int Organ
organCatalog = Map.fromList organPairs

-- Example 
-- GHCi> Map.lookup 7 organCatalog
-- Just Heart

-- Part 1

possibleDrawers :: [Int]
possibleDrawers = [1 .. 50]
  
-- a function to get the contents of each drawer
getDrawerContents :: [Int] -> Map.Map Int Organ -> [Maybe Organ]
getDrawerContents ids catalog = map getContents ids
 where getContents = \id -> Map.lookup id catalog
  
-- search the catalog
availableOrgans :: [Maybe Organ]
availableOrgans = getDrawerContents possibleDrawers organCatalog
 
-- get a count of a particular organ you’re interested in
countOrgan :: Organ -> [Maybe Organ] -> Int
countOrgan organ available = length (filter
                                      (\x -> x == Just organ)
                                      available)

-- Example
-- GHCi> show availableOrgans [Nothing,Just Heart,Nothing,Nothing,Nothing,
-- Nothing,Just Heart,Nothing,Nothing,Nothing...


-- Part 2 - Making a prettier output

-- remove all the Nothing values. 
isSomething :: Maybe Organ -> Bool
isSomething Nothing = False
isSomething (Just _) = True
  
-- filter your list to the organs that aren’t missing
justTheOrgans :: [Maybe Organ]
justTheOrgans = filter isSomething availableOrgans
  
-- clean up Just constructors
-- You’ll add the Nothing pattern even though you won’t need it because
-- it’s a good habit to always match all patterns just in case. 
showOrgan :: Maybe Organ -> String
showOrgan (Just organ) = show organ
showOrgan Nothing = ""
  
-- create the list and make it prettier
organList :: [String]
organList = map showOrgan justTheOrgans
  
cleanList :: String
cleanList = intercalate ", " organList
  
-- Example
-- GHCi> cleanList
-- "Heart, Heart, Brain, Spleen, Spleen, Kidney"