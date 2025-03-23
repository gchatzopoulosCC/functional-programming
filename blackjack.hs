-- Types
data Suit = Hearts | Spades | Diamonds | Clubs
  deriving (Show, Eq)

data Rank = Numeric Int | Jack | Queen | King | Ace
  deriving (Show, Eq)

data Card = Card { rank :: Rank, suit :: Suit }
  deriving (Show, Eq)

type Hand = [Card]

data Player = Bank | Guest
  deriving (Show, Eq)

