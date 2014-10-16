import System.Random
import Data.IORef

data Color = Spade | Diamond | Heart | Club
           deriving (Show, Enum)
data Rank = Ace | Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King
          deriving (Show, Eq,  Ord, Enum)
type Card = (Color, Rank)

type PlayerHands = [[Card]]

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

assignCardsToPlayers :: [Card] -> Int -> Int -> [[Card]]
assignCardsToPlayers _ _ 0 = []
assignCardsToPlayers deck numberOfCards numberOfPlayers =
  (take numberOfCards deck) : (assignCardsToPlayers (drop numberOfCards deck) numberOfCards (numberOfPlayers -1))

getFlop :: [Card] -> [Card]
getFlop deck = take 3 $ drop 1 deck

getTurn :: [Card] -> Card
getTurn deck = head $ drop 1 deck

getRiver :: [Card] -> Card
getRiver = getTurn

main = do
  gen <- getStdGen
  playDeck <- newIORef (shuffleDeck createDeck gen)
  let numberOfCards = 2;
      numberOfPlayers = 4;
  temp <- (readIORef playDeck)
  let playerHands = assignCardsToPlayers temp numberOfCards numberOfPlayers
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
