import Data.Char (chr, isLetter, ord)
import Data.List (sortBy)

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

-- 1
evalCell :: Spreadsheet -> Position -> Double
evalCell sheet position =
  case cellValue sheet position of
    Number x -> x
    Formula f -> f sheet
    Reference r -> evalCell sheet (stringToPosition r)

-- 2
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

--5. countCellsBy that counts the cells that match a predicate.
countCellsBy :: (CellValue -> Bool) -> Spreadsheet -> Int
countCellsBy f [] = 0
countCellsBy f spreadsheet = length (filterCellsByValue f spreadsheet)

--6. sumRange that sums the values of all cells in a given range (e.g., from (1, 1) to (3, 3)).
sumRange :: Spreadsheet -> Position -> Position -> Double
sumRange spreadsheet (x1, y1) (x2, y2) =
  sum
    [ evalCell spreadsheet (x, y)
    | ((x, y), _) <- spreadsheet
    , x >= x1
        && x <= x2
        && ((x == x1 && y >= y1) || (x == x2 && y <= y2) || (x > x1 && x < x2))
    ]

--7 mapRange that applies a numeric function to all cells in a given range.
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

--8 sortCellsByValue that sorts the spreadsheet based on the numeric cell values. The positions of the cells remain unchanged.
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

--4. Implement a function that checks whether a spreadsheet has cyclic references and another that
--   returns the sequences of positions that form cycles (if any). A cyclic reference is a chain of references
--   that returns to itself, e.g., cell A1 references B1, B1 references C1, and C1 references A1.
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
runAllTests :: [(String, Bool)]
runAllTests =
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

-- For easy testing in GHCi
main :: IO ()
main = testSpreadsheet
