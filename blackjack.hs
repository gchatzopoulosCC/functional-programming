-- Types
data Suit = Hearts | Spades | Diamonds | Clubs
  deriving (Show, Eq)


data Rank = Numeric Int | Jack | Queen | King | Ace
  deriving (Show, Eq)

mkNumberic :: Int -> Maybe Rank
mkNumberic n | n >= 2 && n <= 10 = Just (Numeric n)
             | otherwise         = Nothing

data Card = Card { rank :: Rank, suit :: Suit }
  deriving (Show, Eq)


type Hand = [Card]


data Player = Bank | Guest
  deriving (Show, Eq)

