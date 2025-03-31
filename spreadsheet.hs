import Data.List (sortBy)
import Data.Char (isLetter, ord, chr)

-- Types
data CellValue = Number Double
                | Formula (Spreadsheet -> Double)

instance Show CellValue where
    show (Number x) = show x
    show (Formula _) = "Formula"

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
        Number x  -> x
        Formula f -> f sheet


-- 2
updateCell :: Spreadsheet -> Position -> CellValue -> Spreadsheet
updateCell [] position value    = [(position, value)]
updateCell sheet position value =
    let (before, after)         = break (\(pos, _) -> pos == position) sheet
    in case after of
        []            -> before ++ [(position, value)]
        ((_, _):rest) -> before ++ [(position, value)] ++ rest
        

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
    sum [evalCell spreadsheet (x, y) | ((x, y), Number cellValue) <- spreadsheet, x >= x1 && x <= x2 && ((x == x1 && y >= y1) || (x == x2 && y <= y2) || (x > x1 && x < x2))]


--7 mapRange that applies a numeric function to all cells in a given range.
mapRange :: (Double -> Double) -> Spreadsheet -> Position -> Position -> Spreadsheet
mapRange f spreadsheet (x1, y1) (x2, y2) =
    map (\(pos, value) -> 
        if inRange pos 
        then (pos, applyFunction value) 
        else (pos, value)) spreadsheet
  where
    inRange (x, y) = x >= x1 && x <= x2 && y >= y1 && y <= y2
    applyFunction (Number n) = Number (f n)

--8 sortCellsByValue that sorts the spreadsheet based on the numeric cell values. The positions of the cells remain unchanged.

sortCellsByValue :: Spreadsheet -> Spreadsheet
sortCellsByValue spreadsheet = 
    sortedCells = sortBy (\cell1 cell2 -> compare (getCellNumericValue cell1) (getCellNumericValue cell2)) spreadsheet

-- 2. Implement parsing functions that take a string (e.g., “AA1”) and return the position as row and column
-- (1,27) and the reverse operation, i.e., given a position to return the string reference.

stringToPosition :: String -> Position
stringToPosition s = 
  let (letters, numbers) = span isLetter s
      x = read numbers :: Int
      y = convertToBase26 letters
  in (x,y)

reverseStringToPosition :: Position -> String
reverseStringToPosition (x,y) = 
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


--------------------------------------------------------------
-- Testing
--------------------------------------------------------------

-- 1. Simple spreadsheet with numbers only
numbersOnly :: Spreadsheet
numbersOnly = 
    [ ((1, 1), Number 10.0)
    , ((1, 2), Number 20.0)
    , ((2, 1), Number 30.0)
    , ((2, 2), Number 40.0)
    ]

-- 2. Spreadsheet with formulas
-- Formulas that sum all numeric values in the spreadsheet
sumAllNumbers :: Spreadsheet -> Double
sumAllNumbers sheet = sum [x | (_, Number x) <- sheet]

-- Formula that counts all cells
countAllCells :: Spreadsheet -> Double
countAllCells sheet = fromIntegral (length sheet)

formulaSheet :: Spreadsheet
formulaSheet =
    [ ((1, 1), Number 5.0)
    , ((1, 2), Number 10.0)
    , ((2, 1), Formula sumAllNumbers)
    , ((2, 2), Formula countAllCells)
    ]

-- 3. Mixed spreadsheet with numbers and formulas
-- Formula to calculate average
average :: Spreadsheet -> Double
average sheet = 
    let nums = [x | (_, Number x) <- sheet]
    in if null nums then 0 else sum nums / fromIntegral (length nums)

mixedSheet :: Spreadsheet
mixedSheet =
    [ ((1, 1), Number 12.5)
    , ((1, 2), Number 7.3)
    , ((1, 3), Number 9.1)
    , ((2, 1), Formula sumAllNumbers)
    , ((2, 2), Formula average)
    , ((2, 3), Number 42.0)
    ]

-- 4. Empty spreadsheet
emptySheet :: Spreadsheet
emptySheet = []

-- 5. Sparse spreadsheet (non-contiguous positions)
sparseSheet :: Spreadsheet
sparseSheet =
    [ ((100, 100), Number 1.0)
    , ((200, 200), Number 2.0)
    , ((300, 300), Formula sumAllNumbers)
    ]

-- 6. Formula that references specific cells
-- Helper function to get a cell value as Double
getCellValue :: Spreadsheet -> Position -> Double
getCellValue sheet pos =
    case lookup pos sheet of
        Just (Number x) -> x
        _ -> 0.0  -- default value if not found or is a formula

-- Formula that sums two specific cells
sumTwoCells :: Position -> Position -> Spreadsheet -> Double
sumTwoCells pos1 pos2 sheet = getCellValue sheet pos1 + getCellValue sheet pos2

cellReferenceSheet :: Spreadsheet
cellReferenceSheet =
    [ ((1, 1), Number 15.0)
    , ((1, 2), Number 25.0)
    , ((2, 1), Formula (sumTwoCells (1, 1) (1, 2)))
    , ((2, 2), Number 40.0)
    ]

-- Test function for sumRange
testSumRange :: IO ()
testSumRange = do
  putStrLn "Testing sumRange function:"
  
  -- Test case 1: Sum all numeric cells in the spreadsheet
  let totalSum = sumRange testSpreadsheet (0, 0) (2, 2)
  putStrLn $ "Sum of all numeric cells (0,0) to (2,2): " ++ show totalSum
  
  -- Test case 2: Sum cells in top row
  let topRowSum = sumRange testSpreadsheet (0, 0) (0, 2)
  putStrLn $ "Sum of top row (0,0) to (0,2): " ++ show topRowSum
  
  -- Test case 3: Sum cells in first column
  let firstColSum = sumRange testSpreadsheet (0, 0) (2, 0)
  putStrLn $ "Sum of first column (0,0) to (2,0): " ++ show firstColSum
  
  -- Test case 4: Sum cells in 2x2 top-left corner
  let cornerSum = sumRange testSpreadsheet (0, 0) (1, 1) 
  putStrLn $ "Sum of 2x2 top-left corner (0,0) to (1,1): " ++ show cornerSum
  
  -- Test case 5: Single cell
  let singleCell = sumRange testSpreadsheet (2, 2) (2, 2)
  putStrLn $ "Sum of single cell (2,2): " ++ show singleCell
  
  -- Test case 6: Empty range (coordinates reversed)
  let emptyRange = sumRange testSpreadsheet (2, 2) (0, 0)
  putStrLn $ "Sum of empty range (2,2) to (0,0): " ++ show emptyRange
  
  -- Test case 7: Range with negative numbers
  let negativeRange = sumRange testSpreadsheet (2, 1) (2, 2)
  putStrLn $ "Sum of range with negative number (2,1) to (2,2): " ++ show negativeRange

-- Test spreadsheet with various cell values
testSpreadsheet :: Spreadsheet
testSpreadsheet = 
  [ ((0, 0), Number 10.0)
  , ((0, 1), Number 20.0)
  , ((0, 2), Number 30.0)
  , ((1, 0), Number 5.0)
  , ((1, 1), Formula sumAllNumbers)
  , ((1, 2), Formula (\s -> 
      let Number val1 = lookupCell (0, 1) s
          Number val2 = lookupCell (1, 0) s
      in val1 + val2))
  , ((2, 0), Number 7.5)
  , ((2, 1), Number 0.0)
  , ((2, 2), Number (-3.5))
  ]

  -- Helper function to lookup a cell value
lookupCell :: Position -> Spreadsheet -> CellValue
lookupCell pos sheet = 
  case lookup pos sheet of
    Just val -> val
    Nothing  -> Number 0.0  -- Default value for empty cells


testFilterCellsByValue :: IO ()
testFilterCellsByValue = do
  putStrLn "Original spreadsheet:"
  mapM_ print testSpreadsheet
  
  putStrLn "\nOnly cells with values > 5:"
  let filteredSheet = filterCellsByValue isGreaterThanFive testSpreadsheet
  mapM_ print filteredSheet
  where
    isGreaterThanFive (Number n) = n > 5
    isGreaterThanFive _ = False  -- Exclude formulas

-- Test function for mapSpreadsheet
testMapSpreadsheet :: IO ()
testMapSpreadsheet = do
  putStrLn "Testing mapSpreadsheet function:"
  
  putStrLn "Original spreadsheet:"
  mapM_ (\(pos, val) -> putStrLn $ show pos ++ ": " ++ show val) testSpreadsheet
  
  putStrLn "\nAfter doubling all numeric values:"
  let doubledSheet = mapSpreadsheet doubleValue testSpreadsheet
  mapM_ (\(pos, val) -> putStrLn $ show pos ++ ": " ++ show val) doubledSheet
  
  putStrLn "\nAfter applying absolute value to numeric cells:"
  let absSheet = mapSpreadsheet absValue testSpreadsheet
  mapM_ (\(pos, val) -> putStrLn $ show pos ++ ": " ++ show val) absSheet
  
  putStrLn "\nAfter replacing everything with zero (affects both numbers and formulas):"
  let zeroSheet = mapSpreadsheet (\_ -> Number 0.0) testSpreadsheet
  mapM_ (\(pos, val) -> putStrLn $ show pos ++ ": " ++ show val) zeroSheet
  
  putStrLn "\nAfter wrapping formulas (demonstrates formula transformation):"
  let wrappedSheet = mapSpreadsheet wrapFormula testSpreadsheet
  mapM_ (\(pos, val) -> putStrLn $ show pos ++ ": " ++ show val) wrappedSheet
  
  -- Evaluate some cells to verify formula transformation worked
  putStrLn "\nEvaluating a formula cell before and after transformation:"
  let formulaPos = (1, 2)
      originalCell = lookupCell formulaPos testSpreadsheet
      transformedCell = lookupCell formulaPos wrappedSheet
  putStrLn $ "Original formula at " ++ show formulaPos ++ " evaluates to: " 
             ++ show (evaluateCell originalCell testSpreadsheet)
  putStrLn $ "Wrapped formula at " ++ show formulaPos ++ " evaluates to: " 
             ++ show (evaluateCell transformedCell testSpreadsheet)
  where
    -- Transformation functions
    doubleValue (Number n) = Number (n * 2)
    doubleValue formula = formula  -- Keep formulas unchanged
    
    absValue (Number n) = Number (abs n)
    absValue formula = formula  -- Keep formulas unchanged
    
    -- Function that wraps formulas with additional calculation
    wrapFormula (Formula f) = Formula (\s -> let result = f s in result * 2)
    wrapFormula other = other  -- Keep numeric cells unchanged
    
-- Helper to evaluate a cell
evaluateCell :: CellValue -> Spreadsheet -> Double
evaluateCell (Number n) _ = n
evaluateCell (Formula f) sheet = f sheet

-- Helper function to get the numeric value of a cell
getCellNumericValue :: (Position, CellValue) -> Double
getCellNumericValue (, Number n) = n
getCellNumericValue (, Formula f) = f spreadsheet 

-- Test function for countCellsBy
testCountCellsBy :: IO ()
testCountCellsBy = do
  putStrLn "Testing countCellsBy function:"
  
  let numericCount = countCellsBy isNumber testSpreadsheet
  putStrLn $ "Count of numeric cells: " ++ show numericCount
  
  let formulaCount = countCellsBy isFormula testSpreadsheet
  putStrLn $ "Count of formula cells: " ++ show formulaCount
  
  let positiveCount = countCellsBy isPositive testSpreadsheet
  putStrLn $ "Count of positive number cells: " ++ show positiveCount
  
  let negativeCount = countCellsBy isNegative testSpreadsheet
  putStrLn $ "Count of negative number cells: " ++ show negativeCount
  
  let zeroCount = countCellsBy isZero testSpreadsheet
  putStrLn $ "Count of cells with value exactly 0: " ++ show zeroCount
  
  let greaterThan10Count = countCellsBy isGreaterThan10 testSpreadsheet
  putStrLn $ "Count of cells with value > 10: " ++ show greaterThan10Count
  where
    isNumber (Number _) = True
    isNumber _ = False
    
    isFormula (Formula _) = True
    isFormula _ = False
    
    isPositive (Number n) = n > 0
    isPositive _ = False
    
    isNegative (Number n) = n < 0
    isNegative _ = False
    
    isZero (Number n) = n == 0
    isZero _ = False
    
    isGreaterThan10 (Number n) = n > 10
    isGreaterThan10 _ = False

    -- Test function for mapRange
testMapRange :: IO ()
testMapRange = do
  putStrLn "Testing mapRange function:"
  
  putStrLn "Original spreadsheet:"
  mapM_ (\(pos, val) -> putStrLn $ show pos ++ ": " ++ show val) testSpreadsheet
  
  putStrLn "\nAfter doubling numeric values in range (0,0) to (1,1):"
  let doubledSheet = mapRange (*2) testSpreadsheet (0, 0) (1, 1)
  mapM_ (\(pos, val) -> putStrLn $ show pos ++ ": " ++ show val) doubledSheet
  
  putStrLn "\nAfter adding 10 to numeric values in range (0,0) to (2,2):"
  let addedSheet = mapRange (+10) testSpreadsheet (0, 0) (2, 2)
  mapM_ (\(pos, val) -> putStrLn $ show pos ++ ": " ++ show val) addedSheet
  
  putStrLn "\nAfter applying absolute value to numeric values in range (2,0) to (2,2):"
  let absSheet = mapRange abs testSpreadsheet (2, 0) (2, 2)
  mapM_ (\(pos, val) -> putStrLn $ show pos ++ ": " ++ show val) absSheet
  
  putStrLn "\nAfter squaring numeric values in range (0,1) to (0,2):"
  let squaredSheet = mapRange (\x -> x * x) testSpreadsheet (0, 1) (0, 2)
  mapM_ (\(pos, val) -> putStrLn $ show pos ++ ": " ++ show val) squaredSheet
  
  putStrLn "\nAfter applying function to empty range (3,3) to (4,4):"
  let emptyRangeSheet = mapRange (*2) testSpreadsheet (3, 3) (4, 4)
  mapM_ (\(pos, val) -> putStrLn $ show pos ++ ": " ++ show val) emptyRangeSheet
  
  putStrLn "\nAfter applying function to invalid range (2,2) to (0,0):"
  let invalidRangeSheet = mapRange (*2) testSpreadsheet (2, 2) (0, 0)
  mapM_ (\(pos, val) -> putStrLn $ show pos ++ ": " ++ show val) invalidRangeSheet
  
  -- Test with a different spreadsheet that has only numbers
  putStrLn "\nTesting with numbersOnly spreadsheet:"
  putStrLn "Original:"
  mapM_ (\(pos, val) -> putStrLn $ show pos ++ ": " ++ show val) numbersOnly
  
  putStrLn "\nAfter applying function to all cells (1,1) to (2,2):"
  let allNumbersSheet = mapRange (/2) numbersOnly (1, 1) (2, 2)
  mapM_ (\(pos, val) -> putStrLn $ show pos ++ ": " ++ show val) allNumbersSheet

  -- Test function for sortCellsByValue
testSortCellsByValue :: IO ()
testSortCellsByValue = do
  putStrLn "Testing sortCellsByValue function:"
  
  putStrLn "Original spreadsheet:"
  mapM_ printCell testSpreadsheet
  
  putStrLn "\nSorted spreadsheet by cell values:"
  let sortedSheet = sortCellsByValue testSpreadsheet
  mapM_ printCell sortedSheet
  
  putStrLn "\nTesting with numbersOnly spreadsheet:"
  putStrLn "Original:"
  mapM_ printCell numbersOnly
  
  putStrLn "\nSorted by value:"
  let sortedNumbers = sortCellsByValue numbersOnly
  mapM_ printCell sortedNumbers
  where
    printCell (pos, val) = do
      let valueStr = case val of
                        Number n -> show n
                        Formula f -> "Formula: " ++ show (f testSpreadsheet)
      putStrLn $ show pos ++ ": " ++ valueStr

main :: IO ()
main = do
  testMapSpreadsheet
  putStrLn "\n------------------------\n"
  testFilterCellsByValue