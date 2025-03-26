import Data.List (sortOn, groupBy, maximumBy)

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
    month :: Int, --month should not exceed 12
    day   :: Int  --no invalid days
  }
  deriving (Show, Eq, Ord)


-- Smart Constructors
mkDate :: Int -> Int -> Int -> Maybe Date
mkDate year month day = 
      let
            isYearValid               = year > 0 && year <= 2025
            isMonthValid              = month > 0 && month < 13
            isLeapYear                = year `mod` 4 == 0
            isFebuary                 = month == 2
            isMonthWithThirtyOneDays  = month `elem` ([x | x <- [1..7], x `mod` 2 /= 0] ++ [y | y <- [8..12], y `mod` 2 == 0])
            isThirtyOneDaysMonthValid = day > 0 && day <= 31
            isThirtyDaysMonthValid    = day > 0 && day <= 30
            isDayValid                = case isMonthWithThirtyOneDays of
                  True -> isThirtyOneDaysMonthValid -- Check for months which have 31 days
                  _    -> case isLeapYear of        -- Check for months which have less than 31 days
                        True -> if isFebuary then day > 0 && day <= 29 else isThirtyDaysMonthValid -- Leap Year Febuary
                        _    -> if isFebuary then day > 0 && day <= 28 else isThirtyDaysMonthValid -- Non-Leap Year Febuary
      in
            if (isDayValid && isMonthValid && isDayValid) then Just (Date year month day) else Nothing


--1.
employeesWithOverlappingPermits :: [Employee] -> [(EId, EId)]
employeesWithOverlappingPermits emps = [(empId emp1, empId emp2) |
  emp1 <- emps,
  emp2 <- emps,
  empId emp1 < empId emp2,
  Just permit1 <- [permit emp1], 
  Just permit2 <- [permit emp2],
  joinedOn emp1 <= expiryDate permit2 && expiryDate permit1 >= joinedOn emp2
  ]


--2. ehh
employeesByTenure :: [Employee] -> [(Int, [Employee])]
employeesByTenure emps = map formatGroup (groupByTenure (sortByTenure (assignTenure emps)))

assignTenure :: [Employee] -> [(Int, Employee)]
assignTenure emps = [(calculateTenure emp, emp) | emp <- emps]

sortByTenure :: [(Int, Employee)] -> [(Int, Employee)]
sortByTenure = sortOn fst

groupByTenure :: [(Int, Employee)] -> [[(Int, Employee)]]
groupByTenure = groupBy (\(t1, _) (t2, _) -> t1 == t2)

formatGroup :: [(Int, Employee)] -> (Int, [Employee])
formatGroup group = (fst (head group), map snd group)

currDate :: Date
currDate = Date 2025 3 22

yearDiff :: Date -> Date -> Int
yearDiff date1 date2 = year date2 - year date1

calculateTenure :: Employee -> Int
calculateTenure emp =
  let endDate = case leftOn emp of
                    Just date -> date
                    Nothing   -> currDate
  in yearDiff (joinedOn emp) endDate

--3.
longestWorkingEmployee :: [Employee] -> Maybe Employee
longestWorkingEmployee []   = Nothing
longestWorkingEmployee emps = Just (maximumBy (\emp1 emp2 -> compare (calculateTenure emp1) (calculateTenure emp2)) emps)

--4. confident
withExpiredPermit :: [Employee] -> Date -> [EId]
withExpiredPermit emps currDate = [empId emp | emp <- emps, Just p <- [permit emp], currDate > expiryDate p]

--5. confident
avgYearsWorked :: [Employee] -> Double
avgYearsWorked [] = 0
avgYearsWorked emps = if totalEmployees == 0
                      then 0
                      else tenures emps / totalEmployees
  where
   totalEmployees = fromIntegral $ length emps
   tenures []    = 0
   tenures emps  = fromIntegral $ sum [calculateTenure emp | emp <- emps]

testEmployees :: [Employee]
testEmployees = 
  [
    Emp { empId = "E001"
        , joinedOn = Date 2022 1 10
        , permit = Just (Permit "P100" (Date 2024 1 10))
        , leftOn = Nothing
        }
    
  , Emp { empId = "E002"
        , joinedOn = Date 2023 5 15
        , permit = Just (Permit "P200" (Date 2025 6 20))
        , leftOn = Just (Date 2024 6 12)
        }
    
  , Emp { empId = "E003"
        , joinedOn = Date 2022 12 1
        , permit = Just (Permit "P300" (Date 2024 8 30))
        , leftOn = Nothing
        }
    
  , Emp { empId = "E004"
        , joinedOn = Date 2025 7 1
        , permit = Just (Permit "P400" (Date 2027 7 1))
        , leftOn = Nothing
        }
    
  , Emp { empId = "E005"
        , joinedOn = Date 2022 3 15
        , permit = Nothing
        , leftOn = Nothing
        }
    
  , Emp { empId = "E006"
        , joinedOn = Date 2024 1 10
        , permit = Just (Permit "P600" (Date 2026 1 10))
        , leftOn = Nothing
        }
    
  , Emp { empId = "E007"
        , joinedOn = Date 2021 1 08
        , permit = Just (Permit "P700" (Date 2023 1 5))
        , leftOn = Just (Date 2022 11 30)
        }
  ]