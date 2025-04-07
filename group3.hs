-- IMPORTS
import Data.Char (chr, isLetter, ord)
import Data.List (groupBy, maximumBy, sortBy, sortOn)
import Text.Printf (printf)

----------------------------------------------------------------------------------------------------------
-- BLACKJACK
----------------------------------------------------------------------------------------------------------
-- Types
data Suit
  = Hearts
  | Spades
  | Diamonds
  | Clubs
  deriving (Show, Eq, Ord)

data Rank
  = Numeric Integer
  | Jack
  | Queen
  | King
  | Ace
  deriving (Show, Eq, Ord)

data Card = Card
  { rank :: Maybe Rank
  , suit :: Suit
  } deriving (Show, Eq, Ord)

type Hand = [Card]

data Player
  = Bank
  | Guest
  deriving (Show, Eq, Ord)

-- Smart constructors for Rank
mkRank :: Rank -> Maybe Rank
mkRank (Numeric n)
  | n >= 2 && n <= 10 = Just (Numeric n)
  | otherwise = Nothing
mkRank r = Just r

-- Card classification
isFaceCard :: Card -> Bool
isFaceCard (Card rank _) = rank `elem` [mkRank Jack, mkRank Queen, mkRank King]

isAce :: Card -> Bool
isAce (Card (Just Ace) _) = True
isAce (Card _ _) = False

-- Helpers
len :: Hand -> Integer
len [] = 0
len (x:xs) = 1 + len xs

-- We sort aces last so their value is more accurate
sortAcesLast :: Hand -> Hand
sortAcesLast [] = []
sortAcesLast (x:xs) =
  let aces = sortAcesLast [a | a <- xs, isAce a]
      rest = sortAcesLast [a | a <- xs, rank a /= mkRank Ace]
   in rest ++ [x] ++ aces

-- Functionality 
faceCards :: Hand -> Integer
faceCards [] = 0
faceCards (x:xs)
  | isFaceCard x = 1 + faceCards xs
  | otherwise = faceCards xs

value :: Hand -> Integer
value hand = go (sortAcesLast hand) 0
  where
    go [] total = total
    go (x:xs) total =
      case rank x of
        Just (Numeric num) -> go xs (total + num)
        Just Ace ->
          let aceValue =
                if (total + 11 > 21) || len xs > 0
                  then 1
                  else 11
           in go xs (total + aceValue) -- An ace should be 1 if total + 11 > 21 or a second card (Ace) follows after (to prevent cases like: Jack, Ace, Ace being 22)
        _ -> go xs (total + 10)

isBlackjack :: Hand -> Bool
isBlackjack [] = False
isBlackjack hand = len hand == 2 && value hand == 21

gameOver :: Hand -> Bool
gameOver [] = True
gameOver hand = value hand > 21

winner :: Hand -> Hand -> Player
winner player bank
  | gameOver player || (value player <= value bank) = Bank
  | otherwise = Guest

(<+) :: Hand -> Hand -> Hand
(<+) first second = first ++ second

-- Functionality
handSuit :: Suit -> Hand
handSuit suit =
  [Card (mkRank (Numeric n)) suit | n <- [2 .. 10]]
    ++ [ Card (mkRank Jack) suit
       , Card (mkRank Queen) suit
       , Card (mkRank King) suit
       , Card (mkRank Ace) suit
       ]

belongsTo :: Card -> Hand -> Bool
belongsTo card [] = False
belongsTo card (x:xs) = card == x || belongsTo card xs

fullDeck :: Hand
fullDeck =
  handSuit Hearts <+ handSuit Spades <+ handSuit Diamonds <+ handSuit Clubs

draw :: Hand -> Hand -> (Hand, Hand)
draw [] _ = error "Cannot draw from an empty deck"
draw (card:remainingDeck) hand = (remainingDeck, card : hand)

playBank :: Hand -> Hand -> Hand
playBank deck bankHand
  | value bankHand < 16 =
    let (newDeck, newHand) = draw deck bankHand
     in playBank newDeck newHand
  | otherwise = bankHand

----------------------------------------------------------------------------------------------------------
-- EMPLOYEE QUERIES
----------------------------------------------------------------------------------------------------------
-- Types
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

-- 1. A function employeesWithOverlappingPermits :: [Employee] -> [(EId, EId)] that returns a list of
-- unique pairs of employee IDs whose permits overlap. Two permits overlap if: the start date of one
-- permit is before or on the end date of the other, and the end date of one permit is after or on the start
-- date of the other.
employeesWithOverlappingPermits :: [Employee] -> [(EId, EId)]
employeesWithOverlappingPermits emps =
  [ (empId emp1, empId emp2)
  | emp1 <- emps
  , emp2 <- emps
  , empId emp1 < empId emp2 -- Ensure no duplicates are added
  , Just permit1 <- [permit emp1]
  , Just permit2 <- [permit emp2]
  , joinedOn emp1 <= expiryDate permit2 && expiryDate permit1 >= joinedOn emp2
  ]

-- 2. A function employeesByTenure :: [Employee] -> [(Int, [Employee])] that returns a list of tuples
-- where the first element is the tenure (measured in years) and the second is the list of employees. It
-- essentially groups the employees by their tenure.
employeesByTenure :: [Employee] -> [(Int, [Employee])]
employeesByTenure emps = map assignTenure (groupByTenure emps)

-- 3. A function longestWorkingEmployee :: [Employee] -> Maybe Employee that returns the employee
-- that has worked the most amount of time.
longestWorkingEmployee :: [Employee] -> Maybe Employee
longestWorkingEmployee [] = Nothing
longestWorkingEmployee emps =
  Just
    (maximumBy
       (\emp1 emp2 -> compare (calculateTenure emp1) (calculateTenure emp2))
       emps)

-- 4. A function withExpiredPermit :: [Employee] -> Date -> [EId] that given the current date returns the
-- ids of employees with an expired permit.
withExpiredPermit :: [Employee] -> Date -> [EId]
withExpiredPermit emps currDate =
  [empId emp | emp <- emps, Just p <- [permit emp], currDate > expiryDate p]

-- 5. A function avgYearsWorked :: [Employee] -> Double that returns the average years an employee
-- worked in the company. Consider only employees who have left.
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

----------------------------------------------------------------------------------------------------------
-- SPREADSHEET MODELING 
----------------------------------------------------------------------------------------------------------
-- Types
data CellValue
  = Number Double
  | Formula (Spreadsheet -> Double)
  | Reference String

instance Show CellValue where
  show (Number x) = show x
  show (Formula _) = "Formula"
  show (Reference r) = r

type Position = (Int, Int) -- (Row, Column)

type Spreadsheet = [(Position, CellValue)]

-- Helpers
cellValue :: Spreadsheet -> Position -> CellValue
cellValue [] _ = error "The position doesn't exist in the spreadsheet"
cellValue ((pos, value):tail) position =
  if pos == position
    then value
    else cellValue tail position

-- 1. evalCell that evaluates the value of a cell in given the position of the spreadsheet.
evalCell :: Spreadsheet -> Position -> Double
evalCell sheet position =
  case cellValue sheet position of
    Number x -> x
    Formula f -> f sheet
    Reference r -> evalCell sheet (stringToPosition r)

-- 2. updateCell that updates or adds a cell in the spreadsheet in the given position.
updateCell :: Spreadsheet -> Position -> CellValue -> Spreadsheet
updateCell [] position value = [(position, value)]
updateCell sheet position value =
  let (before, after) = break (\(pos, _) -> pos == position) sheet
   in case after of
        [] -> before ++ [(position, value)]
        ((_, _):rest) -> before ++ [(position, value)] ++ rest

-- 3. mapSpreadsheet that applies a function to all cells in the spreadsheet.
mapSpreadsheet :: (CellValue -> CellValue) -> Spreadsheet -> Spreadsheet
mapSpreadsheet f spreadsheet =
  map (\(position, value) -> (position, f value)) spreadsheet

-- 4. filterCellsByValue that filters cell values that match a predicate on the given spreadsheet.
filterCellsByValue :: (CellValue -> Bool) -> Spreadsheet -> Spreadsheet
filterCellsByValue f spreadsheet = filter (\(_, value) -> f value) spreadsheet

-- 5. countCellsBy that counts the cells that match a predicate.
countCellsBy :: (CellValue -> Bool) -> Spreadsheet -> Int
countCellsBy f [] = 0
countCellsBy f spreadsheet = length (filterCellsByValue f spreadsheet)

-- 6. sumRange that sums the values of all cells in a given range (e.g., from (1, 1) to (3, 3)).
sumRange :: Spreadsheet -> Position -> Position -> Double
sumRange spreadsheet (x1, y1) (x2, y2) =
  sum
    [ evalCell spreadsheet (x, y)
    | ((x, y), _) <- spreadsheet
    , x >= x1
        && x <= x2
        && ((x == x1 && y >= y1) || (x == x2 && y <= y2) || (x > x1 && x < x2))
    ]

-- 7. mapRange that applies a numeric function to all cells in a given range.
mapRange ::
     (Double -> Double) -> Spreadsheet -> Position -> Position -> Spreadsheet
mapRange f spreadsheet (x1, y1) (x2, y2) =
  map
    (\(pos, value) ->
       if inRange pos
         then (pos, applyFunction value)
         else (pos, value))
    spreadsheet
  where
    inRange (x, y) =
      x >= x1
        && x <= x2
        && ((x == x1 && y >= y1) || (x == x2 && y <= y2) || (x > x1 && x < x2))
    applyFunction (Number n) = Number (f n)

-- 8. sortCellsByValue that sorts the spreadsheet based on the numeric cell values. The positions of the cells remain unchanged.
sortCellsByValue :: Spreadsheet -> Spreadsheet
sortCellsByValue spreadsheet =
  sortBy
    (\(pos1, _) (pos2, _) ->
       compare (evalCell spreadsheet pos1) (evalCell spreadsheet pos2))
    spreadsheet

-- 2. Implement parsing functions that take a string (e.g., “AA1”) and return the position as row and column
-- (1,27) and the reverse operation, i.e., given a position to return the string reference.
stringToPosition :: String -> Position
stringToPosition s =
  let (letters, numbers) = span isLetter s
      x = read numbers :: Int
      y = convertToBase26 letters
   in (x, y)

positionToString :: Position -> String
positionToString (x, y) = convertFromBase26 y ++ show x

convertToBase26 :: String -> Int
convertToBase26 [] = 0
convertToBase26 (c:cs) =
  (ord c - ord 'A' + 1) * (26 ^ length cs) + convertToBase26 cs

convertFromBase26 :: Int -> String
convertFromBase26 0 = ""
convertFromBase26 n =
  let newN = n - 1
      last = chr (ord 'A' + (newN `mod` 26))
      rest = newN `div` 26
   in convertFromBase26 rest ++ [last]

-- 4. Implement a function that checks whether a spreadsheet has cyclic references and another that
-- returns the sequences of positions that form cycles (if any). A cyclic reference is a chain of references
-- that returns to itself, e.g., cell A1 references B1, B1 references C1, and C1 references A1.
hasCyclicReferences :: Spreadsheet -> Bool
hasCyclicReferences sheet = not $ null $ findCyclicReferences sheet

findCyclicReferences :: Spreadsheet -> [[Position]]
findCyclicReferences sheet =
  let positions = map fst sheet
      cycles = [findCycleFrom sheet pos [] | pos <- positions]
   in filter (not . null) cycles

-- Helper function to find a cycle starting from a specific position
findCycleFrom :: Spreadsheet -> Position -> [Position] -> [Position]
findCycleFrom sheet pos visited
  | pos `elem` visited = dropWhile (/= pos) (visited ++ [pos])
  | otherwise =
    case lookup pos sheet of
      Nothing -> []
      Just cellValue ->
        case getReferencedPosition sheet cellValue of
          Nothing -> []
          Just nextPos -> findCycleFrom sheet nextPos (pos : visited)

-- Helper to extract the position referenced by a cell (if any)
getReferencedPosition :: Spreadsheet -> CellValue -> Maybe Position
getReferencedPosition sheet (Reference ref) = Just $ stringToPosition ref
getReferencedPosition sheet (Formula f) = Nothing
getReferencedPosition _ _ = Nothing

----------------------------------------------------------------------------------------------------------
-- TESTERS
----------------------------------------------------------------------------------------------------------
-- Test data
testCard1 :: Card
testCard1 = Card (mkRank (Numeric 10)) Hearts

testCard2 :: Card
testCard2 = Card (mkRank Jack) Spades

testCard3 :: Card
testCard3 = Card (mkRank Ace) Diamonds

testCard4 :: Card
testCard4 = Card (mkRank (Numeric 5)) Clubs

testCard5 :: Card
testCard5 = Card (mkRank Queen) Hearts

-- Test hands
emptyHand :: Hand
emptyHand = []

blackjackHand :: Hand
blackjackHand = [testCard1, testCard3] -- 10 + Ace (as 11) = 21

bustHand :: Hand
bustHand = [testCard1, testCard2, testCard4] -- 10 + 10 + 5 = 25

safeHand :: Hand
safeHand = [testCard4, testCard4] -- 5 + 5 = 10

twoAcesHand :: Hand
twoAcesHand = [testCard3, testCard3] -- Ace + Ace = 12 (one 11, one 1)

highHand :: Hand
highHand = [testCard1, testCard5] -- 10 + 10 = 20

-- Test decks
smallDeck :: Hand
smallDeck = [testCard1, testCard2, testCard3]

-- Test functions
testIsFaceCard :: Bool
testIsFaceCard =
  and
    [ isFaceCard testCard2 == True -- Jack is face card
    , isFaceCard testCard5 == True -- Queen is face card
    , isFaceCard testCard1 == False -- 10 is not face card
    , isFaceCard testCard3 == False -- Ace is not face card
    ]

testIsAce :: Bool
testIsAce =
  and
    [ isAce testCard3 == True -- Ace of Diamonds
    , isAce testCard1 == False -- 10 of Hearts
    , isAce testCard2 == False -- Jack of Spades
    ]

testLen :: Bool
testLen = and [len emptyHand == 0, len blackjackHand == 2, len bustHand == 3]

testSortAcesLast :: Bool
testSortAcesLast =
  let mixedHand = [testCard3, testCard1, testCard3, testCard2] -- Ace, 10, Ace, Jack
      sortedHand = sortAcesLast mixedHand
   in last sortedHand == testCard3
        && (sortedHand !! (length sortedHand - 2)) == testCard3

testFaceCards :: Bool
testFaceCards =
  and
    [ faceCards emptyHand == 0
    , faceCards blackjackHand == 0 -- 10, Ace (no face cards)
    , faceCards bustHand == 1 -- 10, Jack, 5 (1 face card)
    , faceCards [testCard2, testCard5] == 2 -- Jack, Queen (2 face cards)
    ]

testValue :: Bool
testValue =
  and
    [ value emptyHand == 0
    , value blackjackHand == 21 -- 10 + Ace(11) = 21
    , value bustHand == 25 -- 10 + 10 + 5 = 25
    , value safeHand == 10 -- 5 + 5 = 10
    , value twoAcesHand == 12 -- Ace(11) + Ace(1) = 12
    , value highHand == 20 -- 10 + 10 = 20
    ]

testIsBlackjack :: Bool
testIsBlackjack =
  and
    [ isBlackjack blackjackHand == True -- 10 + Ace = 21 with 2 cards
    , isBlackjack bustHand == False -- > 21
    , isBlackjack safeHand == False -- < 21
    , isBlackjack (testCard1 : testCard1 : testCard1 : []) == False -- 10+10+1=21 but 3 cards
    ]

testGameOver :: Bool
testGameOver =
  and
    [ gameOver emptyHand == True -- Empty hand is game over
    , gameOver blackjackHand == False -- 21 is not game over
    , gameOver bustHand == True -- 25 is game over
    , gameOver safeHand == False -- 10 is not game over
    ]

testWinner :: Bool
testWinner =
  and
    [ winner bustHand blackjackHand == Bank -- Player busts, bank wins
    , winner blackjackHand highHand == Guest -- Player has BJ (21), bank has 20
    , winner highHand blackjackHand == Bank -- Player has 20, bank has BJ (21)
    , winner safeHand highHand == Bank -- Player has 10, bank has 20
    , winner highHand safeHand == Guest -- Player has 20, bank has 10
    ]

testAppend :: Bool
testAppend =
  let combined = safeHand <+ highHand
   in length combined == length safeHand + length highHand

testHandSuit :: Bool
testHandSuit =
  let hearts = handSuit Hearts
   in length hearts == 13 && all (\card -> suit card == Hearts) hearts

testBelongsTo :: Bool
testBelongsTo =
  and
    [ belongsTo testCard1 (testCard1 : testCard2 : []) == True
    , belongsTo testCard3 (testCard1 : testCard2 : []) == False
    ]

testFullDeck :: Bool
testFullDeck =
  and
    [ length fullDeck == 52
    , length (filter (\card -> suit card == Hearts) fullDeck) == 13
    , length (filter (\card -> suit card == Spades) fullDeck) == 13
    , length (filter (\card -> suit card == Diamonds) fullDeck) == 13
    , length (filter (\card -> suit card == Clubs) fullDeck) == 13
    ]

testDraw :: Bool
testDraw =
  let (remainingDeck, newHand) = draw smallDeck emptyHand
   in length remainingDeck == length smallDeck - 1
        && length newHand == 1
        && head newHand == head smallDeck

testPlayBank :: Bool
testPlayBank =
  let bankHand = playBank fullDeck []
   in value bankHand >= 16

-- Run all tests
runAllTestsBlackjack :: [(String, Bool)]
runAllTestsBlackjack =
  [ ("isFaceCard", testIsFaceCard)
  , ("isAce", testIsAce)
  , ("len", testLen)
  , ("sortAcesLast", testSortAcesLast)
  , ("faceCards", testFaceCards)
  , ("value", testValue)
  , ("isBlackjack", testIsBlackjack)
  , ("gameOver", testGameOver)
  , ("winner", testWinner)
  , ("append", testAppend)
  , ("handSuit", testHandSuit)
  , ("belongsTo", testBelongsTo)
  , ("fullDeck", testFullDeck)
  , ("draw", testDraw)
  , ("playBank", testPlayBank)
  ]

-- Main test function
testBlackjack :: IO ()
testBlackjack = do
  putStrLn "Running blackjack tests..."
  let results = runAllTestsBlackjack
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
runAllTestsEmpQueries :: [(String, Bool)]
runAllTestsEmpQueries =
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
  let results = runAllTestsEmpQueries
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

-- Test Data - Sample Spreadsheets
testSheet1 :: Spreadsheet
testSheet1 =
  [ ((1, 1), Number 5)
  , ((1, 2), Number 10)
  , ((2, 1), Number 15)
  , ((2, 2), Formula (\s -> evalCell s (1, 1) + evalCell s (1, 2)))
  , ((3, 1), Reference "A1")
  ]

testSheetCyclic :: Spreadsheet
testSheetCyclic =
  [ ((1, 1), Number 5)
  , ((1, 2), Reference "B1")
  , ((2, 1), Reference "A2")
  , ((2, 2), Reference "A1")
  ]

testSheetRange :: Spreadsheet
testSheetRange =
  [ ((1, 1), Number 1)
  , ((1, 2), Number 2)
  , ((1, 3), Number 3)
  , ((2, 1), Number 4)
  , ((2, 2), Number 5)
  , ((2, 3), Number 6)
  , ((3, 1), Number 7)
  , ((3, 2), Number 8)
  , ((3, 3), Number 9)
  ]

-- Test Functions
-- Test for evalCell
testEvalCell :: Bool
testEvalCell =
  and
    [ evalCell testSheet1 (1, 1) == 5
    , evalCell testSheet1 (1, 2) == 10
    , evalCell testSheet1 (2, 2) == 15 -- Formula cell = A1 + A2
    , evalCell testSheet1 (3, 1) == 5 -- Reference to A1
    ]

-- Test for updateCell
testUpdateCell :: Bool
testUpdateCell =
  and [evalCell (updateCell testSheet1 (1, 1) (Number 100)) (1, 1) == 100]

-- Test for mapSpreadsheet
testMapSpreadsheet :: Bool
testMapSpreadsheet =
  and
    [ let doubled =
            mapSpreadsheet
              (\v ->
                 case v of
                   Number x -> Number (x * 2)
                   _ -> v)
              testSheet1
       in evalCell doubled (1, 1) == 10 && evalCell doubled (1, 2) == 20
    ]

-- Test for filterCellsByValue
testFilterCellsByValue :: Bool
testFilterCellsByValue =
  and
    [ length (filterCellsByValue isNumber testSheet1) == 3
    , length (filterCellsByValue isFormula testSheet1) == 1
    , length (filterCellsByValue isReference testSheet1) == 1
    ]
  where
    isNumber (Number _) = True
    isNumber _ = False
    isFormula (Formula _) = True
    isFormula _ = False
    isReference (Reference _) = True
    isReference _ = False

-- Test for countCellsBy
testCountCellsBy :: Bool
testCountCellsBy =
  and
    [ countCellsBy isNumber testSheet1 == 3
    , countCellsBy isFormula testSheet1 == 1
    ]
  where
    isNumber (Number _) = True
    isNumber _ = False
    isFormula (Formula _) = True
    isFormula _ = False

-- Test for sumRange
testSumRange :: Bool
testSumRange =
  and
    [ sumRange testSheetRange (1, 2) (3, 1) == 27 -- 2+3+4+5+6+7
    ]

-- Test for mapRange
testMapRange :: Bool
testMapRange =
  and
    [ let doubled = mapRange (* 2) testSheetRange (1, 1) (2, 2)
       in evalCell doubled (1, 1) == 2 && evalCell doubled (2, 2) == 10
    ]

-- Test for sortCellsByValue
testSortCellsByValue :: Bool
testSortCellsByValue =
  let sorted = sortCellsByValue testSheet1
      values = map (evalCell testSheet1 . fst) sorted
   in values == [5, 5, 10, 15, 15] -- 5 appears twice due to original and reference

-- Test for string/position conversion
testStringConversion :: Bool
testStringConversion =
  and
    [ stringToPosition "A1" == (1, 1)
    , stringToPosition "B10" == (10, 2)
    , stringToPosition "AA1" == (1, 27)
    , positionToString (1, 1) == "A1"
    , positionToString (10, 2) == "B10"
    , positionToString (1, 27) == "AA1"
    ]

-- Test for cycle detection
testCycleDetection :: Bool
testCycleDetection =
  and
    [ not (hasCyclicReferences testSheet1)
    , hasCyclicReferences testSheetCyclic
    , length (findCyclicReferences testSheetCyclic) > 0
    ]

-- Run all tests
runAllTestsSpreadsheet :: [(String, Bool)]
runAllTestsSpreadsheet =
  [ ("evalCell", testEvalCell)
  , ("updateCell", testUpdateCell)
  , ("mapSpreadsheet", testMapSpreadsheet)
  , ("filterCellsByValue", testFilterCellsByValue)
  , ("countCellsBy", testCountCellsBy)
  , ("sumRange", testSumRange)
  , ("mapRange", testMapRange)
  , ("sortCellsByValue", testSortCellsByValue)
  , ("stringConversion", testStringConversion)
  , ("cycleDetection", testCycleDetection)
  ]

-- Main test function
testSpreadsheet :: IO ()
testSpreadsheet = do
  putStrLn "Running spreadsheet tests..."
  let results = runAllTestsSpreadsheet
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
  return ()

-- For easy testing in GHCi
main :: IO ()
main = do
  putStrLn "=================== BLACKJACK TESTS ==================="
  blackjackResults <- testBlackjack
  putStrLn "\n=================== EMPLOYEE QUERIES TESTS ==================="
  employeeResults <- testEmployeeQueries
  putStrLn "\n=================== SPREADSHEET TESTS ==================="
  spreadsheetResults <- testSpreadsheet
  let blackjackPassed = all snd runAllTestsBlackjack
  let employeePassed = all snd runAllTestsEmpQueries
  let spreadsheetPassed = all snd runAllTestsSpreadsheet
  putStrLn "\n=================== SUMMARY ==================="
  putStrLn
    $ "Blackjack Tests: "
        ++ (if blackjackPassed
              then "ALL PASS"
              else "SOME FAILED")
  putStrLn
    $ "Employee Tests: "
        ++ (if employeePassed
              then "ALL PASS"
              else "SOME FAILED")
  putStrLn
    $ "Spreadsheet Tests: "
        ++ (if spreadsheetPassed
              then "ALL PASS"
              else "SOME FAILED")
  if blackjackPassed && employeePassed && spreadsheetPassed
    then putStrLn "\nALL TESTS PASSED SUCCESSFULLY! ✅"
    else putStrLn "\nSOME TESTS FAILED! ❌"
