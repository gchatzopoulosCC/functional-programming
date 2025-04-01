import Data.List (sortBy)
import Data.Char (isLetter, ord, chr)

-- Types
data CellValue = Number Double
                | Formula (Spreadsheet -> Double)
                | Reference String

instance Show CellValue where
    show (Number x)    = show x
    show (Formula _)   = "Formula"
    show (Reference r) = r

type Position = (Int, Int) -- (Row, Column)
type Spreadsheet = [(Position, CellValue)]


-- Helpers
cellValue :: Spreadsheet -> Position -> CellValue
cellValue [] _                         = error "The position doesn't exist in the spreadsheet"
cellValue ((pos, value):tail) position = if pos == position then value else cellValue tail position


-- 1
evalCell :: Spreadsheet -> Position -> Double
evalCell sheet position =
    case cellValue sheet position of
        Number x    -> x
        Formula f   -> f sheet
        Reference r -> evalCell sheet (stringToPosition r)


-- 2
updateCell :: Spreadsheet -> Position -> CellValue -> Spreadsheet
updateCell [] position value    = [(position, value)]
updateCell sheet position value =
    let (before, after)         = break (\(pos, _) -> pos == position) sheet
    in case after of
        []            -> before ++ [(position, value)]
        ((_, _):rest) -> before ++ [(position, value)] ++ rest
        
updateCellWithReference :: Spreadsheet -> String -> CellValue -> Spreadsheet
updateCellWithReference sheet reference value = 
  updateCell sheet (stringToPosition reference) value

-- 3. mapSpreadsheet that applies a function to all cells in the spreadsheet. (2 pts)
mapSpreadsheet :: (CellValue -> CellValue) -> Spreadsheet -> Spreadsheet
mapSpreadsheet f spreadsheet = map (\(position, value) -> (position, f value)) spreadsheet


-- 4. filterCellsByValue that filters cell values that match a predicate on the given spreadsheet. (4 pts)
filterCellsByValue :: (CellValue -> Bool) -> Spreadsheet -> Spreadsheet
filterCellsByValue f spreadsheet = filter (\(_, value) -> f value) spreadsheet


--5. countCellsBy that counts the cells that match a predicate. (2 pts)
countCellsBy :: (CellValue -> Bool) -> Spreadsheet -> Int
countCellsBy f []          = 0
countCellsBy f spreadsheet = length (filterCellsByValue f spreadsheet)


--6. sumRange that sums the values of all cells in a given range (e.g., from (1, 1) to (3, 3)). (4 pts)
sumRange :: Spreadsheet -> Position -> Position -> Double
sumRange spreadsheet (x1, y1) (x2, y2) = 
    sum [evalCell spreadsheet (x, y) | ((x, y), _) <- spreadsheet, x >= x1 && x <= x2 && ((x == x1 && y >= y1) || (x == x2 && y <= y2) || (x > x1 && x < x2))]

sumRangeWithReference :: Spreadsheet -> String -> String -> Double
sumRangeWithReference spreadsheet start end = 
  sumRange spreadsheet (stringToPosition start) (stringToPosition end)


--7 mapRange that applies a numeric function to all cells in a given range.
mapRange :: (Double -> Double) -> Spreadsheet -> Position -> Position -> Spreadsheet
mapRange f spreadsheet (x1, y1) (x2, y2) =
    map (\(pos, value) -> 
        if inRange pos 
        then (pos, applyFunction value) 
        else (pos, value)) spreadsheet
  where
    inRange (x, y) = x >= x1 && x <= x2 && ((x == x1 && y >= y1) || (x == x2 && y <= y2) || (x > x1 && x < x2))
    applyFunction (Number n) = Number (f n)

mapRangeWithReference :: (Double -> Double) -> Spreadsheet -> String -> String -> Spreadsheet
mapRangeWithReference f spreadsheet start end =
  mapRange f spreadsheet (stringToPosition start) (stringToPosition end)

--8 sortCellsByValue that sorts the spreadsheet based on the numeric cell values. The positions of the cells remain unchanged.

sortCellsByValue :: Spreadsheet -> Spreadsheet
sortCellsByValue spreadsheet = 
    sortBy (\(pos1, _) (pos2, _) -> compare (evalCell spreadsheet pos1) (evalCell spreadsheet pos2)) spreadsheet

-- 2. Implement parsing functions that take a string (e.g., “AA1”) and return the position as row and column
-- (1,27) and the reverse operation, i.e., given a position to return the string reference.

stringToPosition :: String -> Position
stringToPosition s = 
  let (letters, numbers) = span isLetter s
      x = read numbers :: Int
      y = convertToBase26 letters
  in (x,y)

positionToString :: Position -> String
positionToString (x,y) = 
   convertFromBase26 y ++ show x

-- Helper (Could also be done using foldl meaning no helper have to ask prof)

convertToBase26 :: String -> Int
convertToBase26 []     = 0
convertToBase26 (c:cs) = (ord c - ord 'A' + 1) * (26 ^ length cs) + convertToBase26 cs

convertFromBase26 :: Int -> String
convertFromBase26 0 = ""
convertFromBase26 n = 
  let newN = n - 1
      last = chr (ord 'A' + (newN `mod` 26))
      rest = newN `div` 26
  in convertFromBase26 rest ++ [last]

-- Main test function
main :: IO ()
main = do
  putStrLn "=== Spreadsheet Function Test Suite ==="
  putStrLn "\n1. Basic Spreadsheet Functions"
  testEvalCell
  testUpdateCell
  testUpdateCellWithReference
  
  putStrLn "\n2. Utility Functions"
  testStringToPosition
  testPositionToString
  
  putStrLn "\n3. Higher-Order Functions"
  testMapSpreadsheet
  testFilterCellsByValue
  testCountCellsBy
  
  putStrLn "\n4. Range Operations"
  testSumRange
  testSumRangeWithReference
  testMapRange
  testMapRangeWithReference

  putStrLn "\n5. Sorting Function"
  testSortCellsByValue


-- Create test spreadsheet
testSpreadsheet :: Spreadsheet
testSpreadsheet = 
  [ ((1, 1), Number 10.0)                             -- A1: 10.0
  , ((1, 2), Number 20.0)                             -- B1: 20.0
  , ((1, 3), Number 30.0)                             -- C1: 30.0
  , ((2, 1), Number 5.0)                              -- A2: 5.0
  , ((2, 2), Formula (\s -> 2 * evalCell s (1, 1)))   -- B2: =2*A1 (formula that doubles A1)
  , ((2, 3), Formula (\s -> evalCell s (1, 2) + evalCell s (2, 1))) -- C2: =B1+A2 (formula that adds B1 and A2)
  , ((3, 1), Number 7.5)                              -- A3: 7.5
  , ((3, 2), Number 0.0)                              -- B3: 0.0
  , ((3, 3), Number (-3.5))                           -- C3: -3.5
  , ((4, 1), Reference "A1")                          -- A4: Reference to A1
  , ((4, 2), Formula (\s -> evalCell s (4, 1) * 2))   -- B4: Formula that doubles A4's value
  ]

-- 1. Test evalCell
testEvalCell :: IO ()
testEvalCell = do
  putStrLn "Testing evalCell:"
  putStrLn $ "A1 (Number): " ++ show (evalCell testSpreadsheet (1, 1))  -- Should be 10.0
  putStrLn $ "B2 (Formula): " ++ show (evalCell testSpreadsheet (2, 2)) -- Should be 20.0
  putStrLn $ "C2 (Formula): " ++ show (evalCell testSpreadsheet (2, 3)) -- Should be 25.0
  putStrLn $ "A4 (Reference): " ++ show (evalCell testSpreadsheet (4, 1)) -- Should be 10.0
  putStrLn $ "B4 (Formula with Reference): " ++ show (evalCell testSpreadsheet (4, 2)) -- Should be 20.0

-- 2. Test updateCell
testUpdateCell :: IO ()
testUpdateCell = do
  putStrLn "Testing updateCell:"
  let updatedSheet = updateCell testSpreadsheet (1, 1) (Number 15.0)
  putStrLn $ "A1 after update to 15.0: " ++ show (evalCell updatedSheet (1, 1))
  putStrLn $ "B2 after A1 update (formula should recalculate): " ++ show (evalCell updatedSheet (2, 2))
  
  -- Test adding a new cell
  let expandedSheet = updateCell testSpreadsheet (5, 5) (Number 100.0)
  putStrLn $ "E5 after adding new cell: " ++ show (evalCell expandedSheet (5, 5))

-- 3. Test updateCellWithReference
testUpdateCellWithReference :: IO ()
testUpdateCellWithReference = do
  putStrLn "Testing updateCellWithReference:"
  let updatedSheet = updateCellWithReference testSpreadsheet "D4" (Number 42.0)
  putStrLn $ "D4 after update with reference: " ++ show (evalCell updatedSheet (4, 4))

-- 4. Test stringToPosition and positionToString
testStringToPosition :: IO ()
testStringToPosition = do
  putStrLn "Testing stringToPosition:"
  putStrLn $ "A1 -> " ++ show (stringToPosition "A1")  -- Should be (1,1)
  putStrLn $ "B2 -> " ++ show (stringToPosition "B2")  -- Should be (2,2)
  putStrLn $ "Z10 -> " ++ show (stringToPosition "Z10")  -- Should be (10,26)
  putStrLn $ "AA1 -> " ++ show (stringToPosition "AA1")  -- Should be (1,27)
  putStrLn $ "ZZ99 -> " ++ show (stringToPosition "ZZ99")  -- Should be (99,702)

testPositionToString :: IO ()
testPositionToString = do
  putStrLn "Testing positionToString:"
  putStrLn $ "(1,1) -> " ++ positionToString (1,1)  -- Should be "A1"
  putStrLn $ "(2,2) -> " ++ positionToString (2,2)  -- Should be "B2"
  putStrLn $ "(10,26) -> " ++ positionToString (10,26)  -- Should be "Z10"
  putStrLn $ "(1,27) -> " ++ positionToString (1,27)  -- Should be "AA1"
  putStrLn $ "(99,702) -> " ++ positionToString (99,702)  -- Should be "ZZ99"

  -- Test roundtrip: string -> position -> string
  let testRoundtrip str = str ++ " -> " ++ (positionToString $ stringToPosition str)
  putStrLn "Testing roundtrip conversions:"
  mapM_ (putStrLn . testRoundtrip) ["A1", "B2", "Z10", "AA1", "ZZ99"]

-- 5. Test mapSpreadsheet
testMapSpreadsheet :: IO ()
testMapSpreadsheet = do
  putStrLn "Testing mapSpreadsheet:"
  
  -- Double all numeric values
  let doubledSheet = mapSpreadsheet doubleValue testSpreadsheet
  putStrLn "After doubling numeric values:"
  putStrLn $ "A1: " ++ show (evalCell doubledSheet (1, 1))  -- Should be 20.0
  putStrLn $ "B2 (formula - should still calculate based on doubled A1): " ++ show (evalCell doubledSheet (2, 2))  -- Should be 40.0
  
  -- Apply absolute value to all numeric values
  let absSheet = mapSpreadsheet absValue testSpreadsheet
  putStrLn "After applying absolute value:"
  putStrLn $ "C3 (negative number): " ++ show (evalCell absSheet (3, 3))  -- Should be 3.5
  
  where
    doubleValue (Number n) = Number (n * 2)
    doubleValue x = x  -- Keep formulas unchanged
    
    absValue (Number n) = Number (abs n)
    absValue x = x  -- Keep formulas unchanged

-- 6. Test filterCellsByValue
testFilterCellsByValue :: IO ()
testFilterCellsByValue = do
  putStrLn "Testing filterCellsByValue:"
  
  -- Filter only numeric cells
  let numericCells = filterCellsByValue isNumeric testSpreadsheet
  putStrLn $ "Number of numeric cells: " ++ show (length numericCells)
  
  -- Filter only positive numbers
  let positiveCells = filterCellsByValue isPositive testSpreadsheet
  putStrLn $ "Number of positive numeric cells: " ++ show (length positiveCells)
  
  -- Filter only formula cells
  let formulaCells = filterCellsByValue isFormula testSpreadsheet
  putStrLn $ "Number of formula cells: " ++ show (length formulaCells)
  
  where
    isNumeric (Number _) = True
    isNumeric _ = False
    
    isPositive (Number n) = n > 0
    isPositive _ = False
    
    isFormula (Formula _) = True
    isFormula _ = False

-- 7. Test countCellsBy
testCountCellsBy :: IO ()
testCountCellsBy = do
  putStrLn "Testing countCellsBy:"
  
  let numericCount = countCellsBy isNumber testSpreadsheet
  putStrLn $ "Count of numeric cells: " ++ show numericCount
  
  let formulaCount = countCellsBy isFormula testSpreadsheet
  putStrLn $ "Count of formula cells: " ++ show formulaCount
  
  let referenceCount = countCellsBy isReference testSpreadsheet
  putStrLn $ "Count of reference cells: " ++ show referenceCount
  
  let positiveCount = countCellsBy isPositive testSpreadsheet
  putStrLn $ "Count of positive number cells: " ++ show positiveCount
  
  let negativeCount = countCellsBy isNegative testSpreadsheet
  putStrLn $ "Count of negative number cells: " ++ show negativeCount
  
  where
    isNumber (Number _) = True
    isNumber _ = False
    
    isFormula (Formula _) = True
    isFormula _ = False
    
    isReference (Reference _) = True
    isReference _ = False
    
    isPositive (Number n) = n > 0
    isPositive _ = False
    
    isNegative (Number n) = n < 0
    isNegative _ = False

-- 8. Test sumRange
testSumRange :: IO ()
testSumRange = do
  putStrLn "Testing sumRange:"
  
  -- Sum of all cells
  let totalSum = sumRange testSpreadsheet (1, 1) (3, 3)
  putStrLn $ "Sum of all numeric cells (1,1) to (3,3): " ++ show totalSum
  
  -- Sum of first row
  let row1Sum = sumRange testSpreadsheet (1, 1) (1, 3)
  putStrLn $ "Sum of first row (1,1) to (1,3): " ++ show row1Sum
  
  -- Sum of first column
  let col1Sum = sumRange testSpreadsheet (1, 1) (3, 1)
  putStrLn $ "Sum of first column (1,1) to (3,1): " ++ show col1Sum
  
  -- Sum of 2x2 area
  let areaSum = sumRange testSpreadsheet (1, 1) (2, 2)
  putStrLn $ "Sum of 2x2 area (1,1) to (2,2): " ++ show areaSum

-- 9. Test sumRangeWithReference
testSumRangeWithReference :: IO ()
testSumRangeWithReference = do
  putStrLn "Testing sumRangeWithReference:"
  
  let sum1 = sumRangeWithReference testSpreadsheet "A1" "C3"
  putStrLn $ "Sum of A1:C3: " ++ show sum1
  
  let sum2 = sumRangeWithReference testSpreadsheet "A1" "A3"
  putStrLn $ "Sum of A1:A3: " ++ show sum2

-- 10. Test mapRange
testMapRange :: IO ()
testMapRange = do
  putStrLn "Testing mapRange:"
  
  -- Double all values in first row
  let doubledRow1 = mapRange (*2) testSpreadsheet (1, 1) (1, 3)
  putStrLn "After doubling first row:"
  putStrLn $ "A1: " ++ show (evalCell doubledRow1 (1, 1))  -- Should be 20.0
  putStrLn $ "B1: " ++ show (evalCell doubledRow1 (1, 2))  -- Should be 40.0
  putStrLn $ "C1: " ++ show (evalCell doubledRow1 (1, 3))  -- Should be 60.0
  putStrLn $ "A2 (outside range): " ++ show (evalCell doubledRow1 (2, 1))  -- Should remain 5.0
  
  -- Make all values in range negative
  let negativeValues = mapRange negate testSpreadsheet (2, 1) (3, 2)
  putStrLn "After negating values in range (2,1) to (3,2):"
  putStrLn $ "A2: " ++ show (evalCell negativeValues (2, 1))  -- Should be -5.0
  putStrLn $ "B3: " ++ show (evalCell negativeValues (3, 2))  -- Should be -0.0
  putStrLn $ "A1 (outside range): " ++ show (evalCell negativeValues (1, 1))  -- Should remain 10.0

-- 11. Test mapRangeWithReference
testMapRangeWithReference :: IO ()
testMapRangeWithReference = do
  putStrLn "Testing mapRangeWithReference:"
  
  let doubledFirstRow = mapRangeWithReference (*2) testSpreadsheet "A1" "C1"
  putStrLn "After doubling A1:C1 range:"
  putStrLn $ "A1: " ++ show (evalCell doubledFirstRow (1, 1))
  putStrLn $ "B1: " ++ show (evalCell doubledFirstRow (1, 2))
  putStrLn $ "C1: " ++ show (evalCell doubledFirstRow (1, 3))

-- 12. Test sortCellsByValue
testSortCellsByValue :: IO ()
testSortCellsByValue = do
  putStrLn "Testing sortCellsByValue:"
  
  let sortedSheet = sortCellsByValue testSpreadsheet
  putStrLn "Cells sorted by value (lowest to highest):"
  mapM_ (\(pos, _) -> do
    let val = evalCell testSpreadsheet pos
    let ref = positionToString pos
    putStrLn $ ref ++ ": " ++ show val
    ) sortedSheet