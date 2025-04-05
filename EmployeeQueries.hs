import Data.List (groupBy, maximumBy, sortOn)
import Text.Printf (printf)

type EId = String

data WorkPermit = Permit
  { number :: String
  , expiryDate :: Date
  } deriving (Show, Eq)

data Employee = Emp
  { empId :: EId
  , joinedOn :: Date
  , permit :: Maybe WorkPermit
  , leftOn :: Maybe Date
  } deriving (Show, Eq)

data Date = Date
  { year :: Int
  , month :: Int
  , day :: Int
  } deriving (Show, Eq, Ord)

-- Smart Constructors   
mkDate :: Int -> Int -> Int -> Maybe Date
mkDate year month day =
  let isYearValid = year > 0
      isMonthValid = month > 0 && month < 13
      isLeapYear =
        (year `mod` 400 == 0) || (year `mod` 100 /= 0 && year `mod` 4 == 0)
      isKnuckleMonth = month `elem` [1, 3, 5, 7, 8, 10, 12]
      isDayValid
        | month == 2 =
          day > 0
            && if isLeapYear
                 then day <= 29
                 else day <= 28
        | isKnuckleMonth = day > 0 && day <= 31
        | otherwise = day > 0 && day <= 30
   in if isYearValid && isMonthValid && isDayValid
        then Just (Date year month day)
        else Nothing

-- Helper
currDate :: Date
currDate = Date 2025 3 26

-- Check if two employes work the same years
(===) :: Employee -> Employee -> Bool
(===) emp1 emp2 =
  yearDiff (joinedOn emp1) currDate == yearDiff (joinedOn emp2) currDate

groupByTenure :: [Employee] -> [[Employee]]
groupByTenure emps = groupBy (===) (sortOn tenureYears emps)
  where
    tenureYears emp = yearDiff (joinedOn emp) currDate -- Sort for better grouping

assignTenure :: [Employee] -> (Int, [Employee])
assignTenure emps = (getYear, emps)
  where
    getYear = yearDiff (joinedOn (head emps)) currDate

calculateTenure :: Employee -> Int
calculateTenure emp =
  let endDate =
        case leftOn emp of
          Just date -> date
          Nothing -> currDate
   in yearDiff (joinedOn emp) endDate

yearDiff :: Date -> Date -> Int
yearDiff date1 date2 = year date2 - year date1

-- 1.
employeesWithOverlappingPermits :: [Employee] -> [(EId, EId)]
employeesWithOverlappingPermits emps =
  [ (empId emp1, empId emp2)
  | emp1 <- emps
  , emp2 <- emps
  , empId emp1 < empId emp2
  , Just permit1 <- [permit emp1]
  , Just permit2 <- [permit emp2]
  , joinedOn emp1 <= expiryDate permit2 && expiryDate permit1 >= joinedOn emp2
  ]

-- 2.
employeesByTenure :: [Employee] -> [(Int, [Employee])]
employeesByTenure emps = map assignTenure (groupByTenure emps)

--3.
longestWorkingEmployee :: [Employee] -> Maybe Employee
longestWorkingEmployee [] = Nothing
longestWorkingEmployee emps =
  Just
    (maximumBy
       (\emp1 emp2 -> compare (calculateTenure emp1) (calculateTenure emp2))
       emps)

--4.
withExpiredPermit :: [Employee] -> Date -> [EId]
withExpiredPermit emps currDate =
  [empId emp | emp <- emps, Just p <- [permit emp], currDate > expiryDate p]

--5.
avgYearsWorked :: [Employee] -> Double
avgYearsWorked [] = 0
avgYearsWorked emps =
  if totalEmployees == 0
    then 0
    else tenures emps / totalEmployees
  where
    totalEmployees = fromIntegral $ length emps
    tenures [] = 0
    tenures emps = fromIntegral $ sum [calculateTenure emp | emp <- emps]

-- ===================== TEST DATA =====================
-- Test Dates
date2020 :: Date
date2020 = Date 2020 1 1

date2021 :: Date
date2021 = Date 2021 1 1

date2022 :: Date
date2022 = Date 2022 1 1

date2023 :: Date
date2023 = Date 2023 1 1

date2024 :: Date
date2024 = Date 2024 1 1

date2026 :: Date
date2026 = Date 2026 1 1

date2027 :: Date
date2027 = Date 2027 1 1

-- Test Permits
permit2024 :: WorkPermit
permit2024 = Permit "P1" date2024

permit2026 :: WorkPermit
permit2026 = Permit "P2" date2026

permit2027 :: WorkPermit
permit2027 = Permit "P3" date2027

-- Test Employees
emp1 :: Employee
emp1 = Emp "E1" date2020 (Just permit2024) Nothing

emp2 :: Employee
emp2 = Emp "E2" date2021 (Just permit2026) Nothing

emp3 :: Employee
emp3 = Emp "E3" date2022 (Just permit2027) Nothing

emp4 :: Employee
emp4 = Emp "E4" date2020 Nothing (Just date2023)

emp5 :: Employee
emp5 = Emp "E5" date2021 Nothing Nothing

-- Sample employee lists for testing
testEmps1 :: [Employee]
testEmps1 = [emp1, emp2, emp3]

testEmps2 :: [Employee]
testEmps2 = [emp1, emp2, emp3, emp4, emp5]

-- ===================== TEST FUNCTIONS =====================
-- Test for employeesWithOverlappingPermits
testOverlappingPermits :: Bool
testOverlappingPermits =
  let result1 = employeesWithOverlappingPermits testEmps1
      result2 = employeesWithOverlappingPermits [emp1, emp2]
      expected1 = [("E1", "E2"), ("E1", "E3"), ("E2", "E3")]
      expected2 = [("E1", "E2")]
   in result1 == expected1 && result2 == expected2

-- Test for employeesByTenure
testEmployeesByTenure :: Bool
testEmployeesByTenure =
  let result = employeesByTenure testEmps1
        -- Check structure and content rather than exact order
      hasCorrectTenures =
        all (\tenure -> tenure `elem` [3, 4, 5]) (map fst result)
      hasAllEmployees = sum (map (length . snd) result) == length testEmps1
        -- Check that employees are correctly grouped by tenure
      correctGrouping =
        all
          (\(tenure, emps) ->
             all (\emp -> yearDiff (joinedOn emp) currDate == tenure) emps)
          result
   in hasCorrectTenures && hasAllEmployees && correctGrouping

-- Test for longestWorkingEmployee
testLongestWorkingEmployee :: Bool
testLongestWorkingEmployee =
  let result1 = longestWorkingEmployee testEmps1
      result2 = longestWorkingEmployee testEmps2
      result3 = longestWorkingEmployee []
   in result1 == Just emp1 && result2 == Just emp1 && result3 == Nothing

-- Test for withExpiredPermit
testWithExpiredPermit :: Bool
testWithExpiredPermit =
  let testDate = Date 2025 1 2
      result = withExpiredPermit testEmps1 testDate
   in result == ["E1"] -- Only emp1's permit expired (2024)

-- Test for avgYearsWorked
testAvgYearsWorked :: Bool
testAvgYearsWorked =
  let result1 = avgYearsWorked testEmps1
      result2 = avgYearsWorked testEmps2
      result3 = avgYearsWorked []
        -- testEmps1: All still employed in 2025, joined in 2020, 2021, 2022 -> tenures 5, 4, 3 -> avg 4.0
        -- testEmps2: Same plus emp4 (2020-2023: 3 years) and emp5 (2021-2025: 4 years) -> avg 3.8
   in result1 == 4.0 && result2 == 3.8 && result3 == 0.0

-- Run all tests
runAllTests :: [(String, Bool)]
runAllTests =
  [ ("employeesWithOverlappingPermits", testOverlappingPermits)
  , ("employeesByTenure", testEmployeesByTenure)
  , ("longestWorkingEmployee", testLongestWorkingEmployee)
  , ("withExpiredPermit", testWithExpiredPermit)
  , ("avgYearsWorked", testAvgYearsWorked)
  ]

-- Main test function
testEmployeeQueries :: IO ()
testEmployeeQueries = do
  putStrLn "Running employee query tests..."
  let results = runAllTests
  mapM_
    (\(name, result) ->
       putStrLn
         $ name
             ++ ": "
             ++ if result
                  then "PASS"
                  else "FAIL")
    results
  putStrLn
    $ "Tests passed: "
        ++ show (length (filter snd results))
        ++ "/"
        ++ show (length results)

-- For easy testing
main :: IO ()
main = testEmployeeQueries
