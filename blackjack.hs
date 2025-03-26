-- Types
data Suit = Hearts | Spades | Diamonds | Clubs
  deriving (Show, Eq, Ord)

data Rank = Numeric Integer | Jack | Queen | King | Ace
  deriving (Show, Eq, Ord)

data Card = Card { rank :: Maybe Rank, suit :: Suit }
  deriving (Show, Eq, Ord)

type Hand = [Card]

data Player = Bank | Guest
  deriving (Show, Eq, Ord)


-- Smart constructors for Rank
mkNumeric :: Integer -> Maybe Rank
mkNumeric n | n >= 2 && n <= 10 = Just (Numeric n)
            | otherwise         = Nothing

mkNonNumeric :: Rank -> Maybe Rank
mkNonNumeric rank = Just rank

