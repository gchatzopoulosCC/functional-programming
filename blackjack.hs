-- Types
data Suit
  = Hearts
  | Spades
  | Diamonds
  | Clubs
  deriving (Show, Eq, Ord)

data Rank
  = Numeric Integer
  | Jack
  | Queen
  | King
  | Ace
  deriving (Show, Eq, Ord)

data Card = Card
  { rank :: Maybe Rank
  , suit :: Suit
  } deriving (Show, Eq, Ord)

type Hand = [Card]

data Player
  = Bank
  | Guest
  deriving (Show, Eq, Ord)

-- Smart constructors for Rank
mkRank :: Rank -> Maybe Rank
mkRank (Numeric n)
  | n >= 2 && n <= 10 = Just (Numeric n)
  | otherwise = Nothing
mkRank r = Just r

-- Card classification
isFaceCard :: Card -> Bool
isFaceCard (Card rank _) = rank `elem` [mkRank Jack, mkRank Queen, mkRank King]

isAce :: Card -> Bool
isAce (Card (Just Ace) _) = True
isAce (Card _ _) = False

-- Helpers
len :: Hand -> Integer
len [] = 0
len (x:xs) = 1 + len xs

-- We sort aces last so their value is more accurate
sortAcesLast :: Hand -> Hand
sortAcesLast [] = []
sortAcesLast (x:xs) =
  let aces = sortAcesLast [a | a <- xs, isAce a]
      rest = sortAcesLast [a | a <- xs, rank a /= mkRank Ace]
   in rest ++ [x] ++ aces

-- Functionality (Chatzopoulos)
faceCards :: Hand -> Integer
faceCards [] = 0
faceCards (x:xs)
  | isFaceCard x = 1 + faceCards xs
  | otherwise = faceCards xs

value :: Hand -> Integer
value hand = go (sortAcesLast hand) 0
  where
    go [] total = total
    go (x:xs) total =
      case rank x of
        Just (Numeric num) -> go xs (total + num)
        Just Ace ->
          let aceValue =
                if (total + 11 > 21) || len xs > 0
                  then 1
                  else 11
           in go xs (total + aceValue) -- An ace should be 1 if total + 11 > 21 or a second card (Ace) follows after (to prevent cases like: Jack, Ace, Ace being 22)
        _ -> go xs (total + 10)

isBlackjack :: Hand -> Bool
isBlackjack [] = False
isBlackjack hand = len hand == 2 && value hand == 21

gameOver :: Hand -> Bool
gameOver [] = True
gameOver hand = value hand > 21

winner :: Hand -> Hand -> Player
winner player bank
  | gameOver player || (value player <= value bank) = Bank
  | otherwise = Guest

(<+) :: Hand -> Hand -> Hand
(<+) first second = first ++ second

-- Functionality (Tampakis)
handSuit :: Suit -> Hand
handSuit suit =
  [Card (mkRank (Numeric n)) suit | n <- [2 .. 10]]
    ++ [ Card (mkRank Jack) suit
       , Card (mkRank Queen) suit
       , Card (mkRank King) suit
       , Card (mkRank Ace) suit
       ]

belongsTo :: Card -> Hand -> Bool
belongsTo card [] = False
belongsTo card (x:xs) = card == x || belongsTo card xs

fullDeck :: Hand
fullDeck =
  handSuit Hearts <+ handSuit Spades <+ handSuit Diamonds <+ handSuit Clubs

draw :: Hand -> Hand -> (Hand, Hand)
draw [] _ = error "Cannot draw from an empty deck"
draw (card:remainingDeck) hand = (remainingDeck, card : hand)

playBank :: Hand -> Hand -> Hand
playBank deck bankHand
  | value bankHand < 16 =
    let (newDeck, newHand) = draw deck bankHand
     in playBank newDeck newHand
  | otherwise = bankHand

-- Test data
testCard1 :: Card
testCard1 = Card (mkRank (Numeric 10)) Hearts

testCard2 :: Card
testCard2 = Card (mkRank Jack) Spades

testCard3 :: Card
testCard3 = Card (mkRank Ace) Diamonds

testCard4 :: Card
testCard4 = Card (mkRank (Numeric 5)) Clubs

testCard5 :: Card
testCard5 = Card (mkRank Queen) Hearts

-- Test hands
emptyHand :: Hand
emptyHand = []

blackjackHand :: Hand
blackjackHand = [testCard1, testCard3]  -- 10 + Ace (as 11) = 21

bustHand :: Hand
bustHand = [testCard1, testCard2, testCard4]  -- 10 + 10 + 5 = 25

safeHand :: Hand
safeHand = [testCard4, testCard4]  -- 5 + 5 = 10

twoAcesHand :: Hand
twoAcesHand = [testCard3, testCard3]  -- Ace + Ace = 12 (one 11, one 1)

highHand :: Hand
highHand = [testCard1, testCard5]  -- 10 + 10 = 20

-- Test decks
smallDeck :: Hand
smallDeck = [testCard1, testCard2, testCard3]

-- Test functions
testIsFaceCard :: Bool
testIsFaceCard = and [
  isFaceCard testCard2 == True,    -- Jack is face card
  isFaceCard testCard5 == True,    -- Queen is face card
  isFaceCard testCard1 == False,   -- 10 is not face card
  isFaceCard testCard3 == False    -- Ace is not face card
  ]

testIsAce :: Bool
testIsAce = and [
  isAce testCard3 == True,    -- Ace of Diamonds
  isAce testCard1 == False,   -- 10 of Hearts
  isAce testCard2 == False    -- Jack of Spades
  ]

testLen :: Bool
testLen = and [
  len emptyHand == 0,
  len blackjackHand == 2,
  len bustHand == 3
  ]

testSortAcesLast :: Bool
testSortAcesLast = 
  let mixedHand = [testCard3, testCard1, testCard3, testCard2]  -- Ace, 10, Ace, Jack
      sortedHand = sortAcesLast mixedHand
  in last sortedHand == testCard3 && (sortedHand !! (length sortedHand - 2)) == testCard3
  
testFaceCards :: Bool
testFaceCards = and [
  faceCards emptyHand == 0,
  faceCards blackjackHand == 0,       -- 10, Ace (no face cards)
  faceCards bustHand == 1,            -- 10, Jack, 5 (1 face card)
  faceCards [testCard2, testCard5] == 2 -- Jack, Queen (2 face cards)
  ]

testValue :: Bool
testValue = and [
  value emptyHand == 0,
  value blackjackHand == 21,      -- 10 + Ace(11) = 21
  value bustHand == 25,           -- 10 + 10 + 5 = 25
  value safeHand == 10,           -- 5 + 5 = 10
  value twoAcesHand == 12,        -- Ace(11) + Ace(1) = 12
  value highHand == 20            -- 10 + 10 = 20
  ]

testIsBlackjack :: Bool
testIsBlackjack = and [
  isBlackjack blackjackHand == True,   -- 10 + Ace = 21 with 2 cards
  isBlackjack bustHand == False,       -- > 21
  isBlackjack safeHand == False,       -- < 21
  isBlackjack (testCard1:testCard1:testCard1:[]) == False  -- 10+10+1=21 but 3 cards
  ]

testGameOver :: Bool
testGameOver = and [
  gameOver emptyHand == True,     -- Empty hand is game over
  gameOver blackjackHand == False, -- 21 is not game over
  gameOver bustHand == True,      -- 25 is game over
  gameOver safeHand == False      -- 10 is not game over
  ]

testWinner :: Bool
testWinner = and [
  winner bustHand blackjackHand == Bank,    -- Player busts, bank wins
  winner blackjackHand highHand == Guest,   -- Player has BJ (21), bank has 20
  winner highHand blackjackHand == Bank,    -- Player has 20, bank has BJ (21)
  winner safeHand highHand == Bank,         -- Player has 10, bank has 20
  winner highHand safeHand == Guest         -- Player has 20, bank has 10
  ]

testAppend :: Bool
testAppend = 
  let combined = safeHand <+ highHand
  in length combined == length safeHand + length highHand

testHandSuit :: Bool
testHandSuit = 
  let hearts = handSuit Hearts
  in length hearts == 13 && all (\card -> suit card == Hearts) hearts

testBelongsTo :: Bool
testBelongsTo = and [
  belongsTo testCard1 (testCard1:testCard2:[]) == True,
  belongsTo testCard3 (testCard1:testCard2:[]) == False
  ]

testFullDeck :: Bool
testFullDeck = and [
  length fullDeck == 52,
  length (filter (\card -> suit card == Hearts) fullDeck) == 13,
  length (filter (\card -> suit card == Spades) fullDeck) == 13,
  length (filter (\card -> suit card == Diamonds) fullDeck) == 13,
  length (filter (\card -> suit card == Clubs) fullDeck) == 13
  ]

testDraw :: Bool
testDraw = 
  let (remainingDeck, newHand) = draw smallDeck emptyHand
  in length remainingDeck == length smallDeck - 1 && 
     length newHand == 1 && 
     head newHand == head smallDeck

testPlayBank :: Bool
testPlayBank = 
  let bankHand = playBank fullDeck []
  in value bankHand >= 16

-- Run all tests
runAllTests :: [(String, Bool)]
runAllTests = [
  ("isFaceCard", testIsFaceCard),
  ("isAce", testIsAce),
  ("len", testLen),
  ("sortAcesLast", testSortAcesLast),
  ("faceCards", testFaceCards),
  ("value", testValue),
  ("isBlackjack", testIsBlackjack),
  ("gameOver", testGameOver),
  ("winner", testWinner),
  ("append", testAppend),
  ("handSuit", testHandSuit),
  ("belongsTo", testBelongsTo),
  ("fullDeck", testFullDeck),
  ("draw", testDraw),
  ("playBank", testPlayBank)
  ]

-- Main test function
testBlackjack :: IO ()
testBlackjack = do
  putStrLn "Running blackjack tests..."
  let results = runAllTests
  mapM_ (\(name, result) -> putStrLn $ name ++ ": " ++ if result then "PASS" else "FAIL") results
  putStrLn $ "Tests passed: " ++ show (length (filter snd results)) ++ "/" ++ show (length results)

-- For easy testing
main :: IO ()
main = testBlackjack

