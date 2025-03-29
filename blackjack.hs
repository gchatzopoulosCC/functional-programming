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
isFaceCard (Card rank _) = rank `elem` [ mkRank Jack, mkRank Queen, mkRank King]

isAce :: Card -> Bool
isAce (Card (Just Ace) _) = True
isAce (Card _ _)            = False


-- Helpers
len :: Hand -> Integer
len [] = 0
len (x:xs) = 1 + len xs

-- We sort aces last so their value is more accurate
sortAcesLast :: Hand -> Hand
sortAcesLast [] = []
sortAcesLast (x:xs) = 
    let aces  = sortAcesLast [a | a <- xs, isAce a]
        rest  = sortAcesLast [a | a <- xs, rank a /= mkRank Ace]
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

(<+) :: Hand -> Hand -> Hand
(<+) first second = first ++ second


-- Functionality (Tampakis)
handSuit :: Suit -> Hand
handSuit suit = 
    [Card (mkRank (Numeric n)) suit | n <- [2..10]] ++ 
    [Card (mkRank Jack) suit, Card (mkRank Queen) suit, Card (mkRank King) suit, Card (mkRank Ace) suit]

belongsTo :: Card -> Hand -> Bool
belongsTo card [] = False
belongsTo card (x:xs) = card == x || belongsTo card xs

fullDeck :: Hand
fullDeck = handSuit Hearts <+ handSuit Spades <+ handSuit Diamonds <+ handSuit Clubs

draw :: Hand -> Hand -> (Hand, Hand)
draw [] _ = error "Cannot draw from an empty deck"
draw (card:remainingDeck) hand = (remainingDeck, card:hand)

playBank :: Hand -> Hand -> Hand
playBank deck bankHand 
    | value bankHand < 16 = let (newDeck, newHand) = draw deck bankHand
                            in playBank newDeck newHand
    | otherwise = bankHand


-----------------------------------------------------
-- Test
-----------------------------------------------------
card1 :: Card
card1 = Card (mkRank (Numeric 10)) Hearts

card2 :: Card
card2 = Card (mkRank Ace) Spades

card3 :: Card
card3 = Card (mkRank King) Diamonds

card4 :: Card
card4 = Card (mkRank (Numeric 5)) Clubs

-- Example hands
hand1 :: Hand
hand1 = [card1, card2] -- A hand with 10 and Ace (Blackjack)

hand2 :: Hand
hand2 = [card3, card4] -- A hand with King and 5

hand3 :: Hand
hand3 = [card1, card4, card2] -- A hand with 10, 5, and Ace

-- Example deck
exampleDeck :: Hand
exampleDeck = fullDeck -- Full deck of cards

main :: IO ()
main = do
    -- Test value function
    putStrLn "Testing value function:"
    print $ value hand1 -- Should print 21
    print $ value hand2 -- Should print 15
    print $ value hand3 -- Should print 16

    -- Test isBlackjack function
    putStrLn "\nTesting isBlackjack function:"
    print $ isBlackjack hand1 -- Should print True
    print $ isBlackjack hand2 -- Should print False
    print $ isBlackjack hand3 -- Should print False

    -- Test gameOver function
    putStrLn "\nTesting gameOver function:"
    print $ gameOver hand1 -- Should print False
    print $ gameOver hand2 -- Should print False
    print $ gameOver (hand1 ++ hand2) -- Should print True (value > 21)

    -- Test winner function
    putStrLn "\nTesting winner function:"
    print $ winner hand1 hand2 -- Should print Guest (hand1 has 21, hand2 has 15)
    print $ winner hand2 hand1 -- Should print Bank (hand2 has 15, hand1 has 21)
    print $ winner hand2 (hand1 ++ hand2) -- Should print Bank (hand1 ++ hand2 is over 21)

    -- Test belongsTo function
    putStrLn "\nTesting belongsTo function:"
    print $ belongsTo card1 hand1 -- Should print True
    print $ belongsTo card3 hand1 -- Should print False

    -- Test draw function
    putStrLn "\nTesting draw function:"
    let (newDeck, newHand) = draw exampleDeck hand2
    print newHand -- Should show hand2 with one more card
    print $ len newDeck -- Should be one less than the full deck

    -- Test playBank function
    putStrLn "\nTesting playBank function:"
    print $ playBank exampleDeck [] -- Should simulate the bank's play

    -- Test fullDeck and handSuit
    putStrLn "\nTesting fullDeck and handSuit:"
    print $ len fullDeck -- Should print 52
    print $ len (handSuit Hearts) -- Should print 13
