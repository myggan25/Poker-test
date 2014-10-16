import System.Random
import Data.IORef

data Color = Spade | Diamond | Heart | Club
           deriving (Show, Enum)
data Rank = Ace | Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King
          deriving (Show, Eq,  Ord, Enum)
type Card = (Color, Rank)

createDeck :: [Card]
createDeck = [(color, number) | number <- [Ace .. King], color <- [Spade .. Club]]

shuffleDeck :: [Card] -> StdGen -> [Card]
shuffleDeck [] _ = []
shuffleDeck oldDeck gen = a : shuffleDeck newList gen
  where
    (n,_) = randomR (0, (length oldDeck)-1) gen
    (ys,zs) = splitAt n oldDeck 
    newList = ys ++ (tail zs)
    a = head zs

assignCards :: [Card] -> Int -> Int -> [[Card]]
assignCards _ _ 0 = []
assignCards deck numberOfCards numberOfPlayers =
  (take numberOfCards deck) :
  (assignCards (drop numberOfCards deck) numberOfCards (numberOfPlayers -1))

getFlop :: [Card] -> [Card]
getFlop deck = take 3 $ drop 1 deck

getTurn :: [Card] -> Card
getTurn deck = head $ drop 1 deck

getRiver :: [Card] -> Card
getRiver = getTurn

haveFlush :: [Card] -> Bool
haveFlush cards = findFlushHelp cards 0 0 0 0

findFlushHelp :: [Card] -> Int -> Int -> Int -> Int -> Bool
findFlushHelp [] _ _ _ _ = False
findFlushHelp _ 5 _ _ _  = True
findFlushHelp _ _ 5 _ _  = True
findFlushHelp _ _ _ 5 _  = True
findFlushHelp _ _ _ _ 5  = True
findFlushHelp (x:xs) spade diamond heart club =
  case (x) of
    (Spade,_) -> findFlushHelp xs (spade+1) diamond heart club
    (Diamond,_) -> findFlushHelp xs spade (diamond+1) heart club
    (Heart,_) -> findFlushHelp xs spade diamond (heart+1) club
    (Club,_) -> findFlushHelp xs spade diamond heart (club+1)


main = do
  gen <- newStdGen
  playDeck <- newIORef (shuffleDeck createDeck gen)
  let numberOfCards = 2;
      numberOfPlayers = 4;
  temp <- (readIORef playDeck)
  let playerHands = assignCards temp numberOfCards numberOfPlayers
  writeIORef playDeck (drop (numberOfCards*numberOfPlayers) temp)
  
  putStrLn(show playerHands)

  temp <- (readIORef playDeck)
  let flop = getFlop temp
  writeIORef playDeck (drop 4 temp)
  putStrLn ("Flop: " ++ (show flop))

  temp <- (readIORef playDeck)
  let turn = getTurn temp
  writeIORef playDeck (drop 2 temp)
  putStrLn ("Turn: " ++ (show turn))  

  temp <- (readIORef playDeck)
  let river = getRiver temp
  writeIORef playDeck (drop 2 temp)
  putStrLn ("River: " ++ (show river))

  putStrLn (show (haveFlush ((head playerHands) ++ flop ++ [turn] ++ [river])))
