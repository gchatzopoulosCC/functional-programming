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
mkRank :: Rank -> Maybe Rank
mkRank (Numeric n) | n >= 2 && n <= 10 = Just (Numeric n)
                   | otherwise         = Nothing
mkRank r = Just r


-- Card classification
isFaceCard :: Card -> Bool
isFaceCard (Card rank _) = rank `elem` [ Just Jack, Just Queen, Just King]

isAce :: Card -> Bool
isAce (Card (Just Ace) _) = True
isAce (Card _ _)          = False


-- Helpers
len :: Hand -> Integer
len [] = 0
len (x:xs) = 1 + len xs

-- We sort aces last so their value is more accurate
sortAcesLast :: Hand -> Hand
sortAcesLast [] = []
sortAcesLast (x:xs) = 
    let aces  = sortAcesLast [a | a <- xs, isAce a]
        rest  = sortAcesLast [a | a <- xs, rank a /= Just Ace]
    in  rest ++ [x] ++ aces


-- Functionality (Chatzopoulos)
faceCards :: Hand -> Integer
faceCards [] = 0
faceCards (x:xs) | isFaceCard x = 1 + faceCards xs
                 | otherwise    = faceCards xs

value :: Hand -> Integer
value hand = go (sortAcesLast hand) 0
    where
        go [] total     = total
        go (x:xs) total = case rank x of
            Just (Numeric num) -> go xs (total + num)
            Just Ace           -> let aceValue = if (total + 11 > 21) || len xs > 0 then 1 else 11 
                                  in  go xs (total + aceValue) -- An ace should be 1 if total + 11 > 21 or a second card (Ace) follows after (to prevent cases like: Jack, Ace, Ace being 22)
            _                  -> go xs (total + 10)

isBlackjack :: Hand -> Bool
isBlackjack [] = False
isBlackjack hand = len hand == 2 && value hand == 21

gameOver :: Hand -> Bool
gameOver [] = True
gameOver hand = value hand > 21

winner :: Hand -> Hand -> Player
winner player bank | gameOver player || (value player <= value bank) = Bank
                   | otherwise                                       = Guest
