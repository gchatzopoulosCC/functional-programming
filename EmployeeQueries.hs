import Data.List (sortOn, groupBy, maximumBy)
import Text.Printf (printf)

type EId = String

data WorkPermit = Permit { number :: String, expiryDate :: Date }
   deriving (Show, Eq)

data Employee = Emp
   {
    empId    :: EId,
    joinedOn :: Date,
    permit   :: Maybe WorkPermit,
    leftOn   :: Maybe Date
   }
   deriving (Show, Eq)

data Date = Date 
  { 
    year  :: Int,
    month :: Int,
    day   :: Int
  }
  deriving (Show, Eq, Ord)


-- Smart Constructors   
mkDate :: Int -> Int -> Int -> Maybe Date
mkDate year month day = 
      let
            isYearValid    = year > 0 && year <= 2025
            isMonthValid   = month > 0 && month < 13
            isLeapYear     = (year `mod` 400 == 0) || (year `mod` 100 /= 0 && year `mod` 4 == 0)
            isKnuckleMonth = month `elem` [1, 3, 5, 7, 8, 10, 12]
            isDayValid   | month == 2     = day > 0 && if isLeapYear then day <= 29 else day <= 28
                         | isKnuckleMonth = day > 0 && day <= 31
                         | otherwise      = day > 0 && day <= 30
      in
            if isYearValid && isMonthValid && isDayValid then Just (Date year month day) else Nothing


-- Helper
currDate :: Date
currDate = Date 2025 3 26 

-- Check if two employes work the same years
(===) :: Employee -> Employee -> Bool
(===) emp1 emp2 = (year currDate - year (joinedOn emp1)) == (year currDate - year (joinedOn emp2))

groupByTenure :: [Employee] -> [[Employee]]
groupByTenure emps = groupBy (===) (sortOn tenureYears emps)
      where tenureYears emp = year currDate - year (joinedOn emp) -- Sort for better grouping

assignTenure :: [Employee] -> (Int, [Employee])
assignTenure emps  = 
      let getYear  = year (joinedOn (head emps))
          yearDiff = (year currDate) - getYear
      in  (yearDiff, emps)

calculateTenure :: Employee -> Int
calculateTenure emp =
  let endDate = case leftOn emp of
                    Just date -> date
                    Nothing   -> currDate
  in yearDiff (joinedOn emp) endDate

yearDiff :: Date -> Date -> Int
yearDiff date1 date2 = year date2 - year date1



-- 1.
employeesWithOverlappingPermits :: [Employee] -> [(EId, EId)]
employeesWithOverlappingPermits emps = [(empId emp1, empId emp2) |
  emp1 <- emps,
  emp2 <- emps,
  empId emp1 < empId emp2,
  Just permit1 <- [permit emp1], 
  Just permit2 <- [permit emp2],
  joinedOn emp1 <= expiryDate permit2 && expiryDate permit1 >= joinedOn emp2
  ]


-- 2.
employeesByTenure :: [Employee] -> [(Int, [Employee])]
employeesByTenure emps = map assignTenure (groupByTenure emps)


--3.
longestWorkingEmployee :: [Employee] -> Maybe Employee
longestWorkingEmployee []   = Nothing
longestWorkingEmployee emps = Just (maximumBy (\emp1 emp2 -> compare (calculateTenure emp1) (calculateTenure emp2)) emps)


--4.
withExpiredPermit :: [Employee] -> Date -> [EId]
withExpiredPermit emps currDate = [empId emp | emp <- emps, Just p <- [permit emp], currDate > expiryDate p]


--5.
avgYearsWorked :: [Employee] -> Double
avgYearsWorked [] = 0
avgYearsWorked emps = if totalEmployees == 0
                      then 0
                      else tenures emps / totalEmployees
  where
   totalEmployees = fromIntegral $ length emps
   tenures []    = 0
   tenures emps  = fromIntegral $ sum [calculateTenure emp | emp <- emps]



--------------------------------------------------------------------
-- Testing purposes
--------------------------------------------------------------------

-- Main
main :: IO ()
main = prettyTenureGroups (employeesByTenure testEmployees)


-- Test
testEmployees :: [Employee]
testEmployees = [Emp "E001" (Date 2020 3 15) (Just (Permit "P100" (Date 2023 3 15))) Nothing, Emp "E002" (Date 2021 6 10) (Just (Permit "P200" (Date 2024 6 10))) (Just (Date 2023 12 31)), Emp "E003" (Date 2022 1 5) Nothing Nothing, Emp "E004" (Date 2022 9 22) (Just (Permit "P400" (Date 2019 9 22))) Nothing, Emp "E005" (Date 2023 4 1) (Just (Permit "P500" (Date 2021 4 1))) (Just (Date 2024 4 1))]


-- Print
prettyTenureGroups :: [(Int, [Employee])] -> IO ()
prettyTenureGroups groups = do
  putStrLn "==== Employees by Tenure ===="
  mapM_ printGroup groups
  where
    printGroup (years, emps) = do
      putStrLn $ printf "\n---- %d Year%s Tenure (%d employees) ----" 
        years 
        (if years /= 1 then "s" else "") 
        (length emps)
      mapM_ (putStr . prettyEmployee) emps

prettyEmployeeGroup :: [[Employee]] -> IO ()
prettyEmployeeGroup groups = do
  putStrLn "==== Employee Groups ===="
  mapM_ printGroup (zip [1..] groups)
  where
    printGroup (n, emps) = do
      putStrLn $ "\n---- Group " ++ show n ++ " (" ++ show (length emps) ++ " employees) ----"
      mapM_ (putStr . prettyEmployee) emps

-- Modified version of your existing prettyEmployee
prettyEmployee :: Employee -> String
prettyEmployee emp = unlines
  [ "  Employee ID: " ++ empId emp
  , "  Joined On:   " ++ prettyDate (joinedOn emp)
  , "  Permit:      " ++ case permit emp of
                          Nothing -> "None"
                          Just p  -> number p ++ " (expires " ++ prettyDate (expiryDate p) ++ ")"
  , "  Status:      " ++ case leftOn emp of
                          Nothing -> "Active"
                          Just d  -> "Left on " ++ prettyDate d
  , ""
  ]

prettyDate :: Date -> String
prettyDate (Date y m d) = printf "%04d-%02d-%02d" y m d
