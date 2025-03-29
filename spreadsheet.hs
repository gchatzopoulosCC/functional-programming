data CellValue = Number Double
                | Formula (Spreadsheet -&gt; Double)


instance Show CellValue where
    show (Number x) = show x
    show (Formula _) = &quot;Formula&quot;


type Position = (Int, Int) -- (Row, Column)
type Spreadsheet = [(Position, CellValue)]

