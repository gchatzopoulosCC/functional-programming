data CellValue = Number Double
                | Formula (Spreadsheet -> Double)


instance Show CellValue where
    show (Number x) = show x
    show (Formula _) = "Formula";


type Position = (Int, Int) -- (Row, Column)
type Spreadsheet = [(Position, CellValue)]

-- 3. mapSpreadsheet that applies a function to all cells in the spreadsheet. (2 pts)
mapSpreadsheet :: (CellValue -> CellValue) -> Spreadsheet -> Spreadsheet
mapSpreadsheet f spreadsheet =  map (\(position, value) -> (position, f value)) spreadsheet

-- 4. filterCellsByValue that filters cell values that match a predicate on the given spreadsheet. (4 pts)
filterCellsByValue :: (CellValue -> Bool) -> Spreadsheet -> Spreadsheet
filterCellsByValue f spreadsheet = filter (\(_, value) -> f value) spreadsheet

--5. countCellsBy that counts the cells that match a predicate. (2 pts)
countCellsBy :: (CellValue -> Bool) -> Spreadsheet -> Int
countCellsBy f []           = 0
countCellsBy f spreadsheet = length (filterCellsByValue f spreadsheet)

--6. sumRange that sums the values of all cells in a given range (e.g., from (1, 1) to (3, 3)). (4 pts)
sumRange :: Spreadsheet -> Position -> Position -> Double
sumRange spreadsheet (x1, y1) (x2, y2) = 
    sum [cellValue | ((x, y), Number cellValue) <- spreadsheet, x >= x1, x <= x2, y >= y1, y <= y2]


sumAllNumbers :: Spreadsheet -> Double
sumAllNumbers sheet = sum [x | (_, Number x) <- sheet]


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

main :: IO ()
main = do
  testMapSpreadsheet
  putStrLn "\n------------------------\n"
  testFilterCellsByValue