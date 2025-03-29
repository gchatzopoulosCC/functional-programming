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
    sum [cellValue | ((x, y), Number cellValue) <- spreadsheet, x >= x1, x <= x2, y >= y1, y <= y2]




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
