module EOIO where
 
{- IO for EO solitaire 
   display an EOBoard
   display a list of EOBoards
play a game, displaying successive moves -}



 -- import your solitaire code here
 
 import Solitaire 
 import Data.Maybe
 import System.Random
 import Data.List
 import Data.Function
 import Data.Ord
 import Debug.Trace
 
{-  Data Structures to be imported
 -- playing card data structures

 data Suit = Hearts|Clubs|Diamonds|Spades
             deriving (Eq, Show)
             
 data Pip = Ace|Two|Three|Four|Five|Six|Seven|Eight|Nine|Ten|Jack|Queen|King
            deriving (Eq,Ord,Show,Enum)
            
 type Card = (Pip,Suit)
 
 type Deck = [Card]
 
 ------------------------------------------------------------------  
-- 8 off solitaire data structures

 type EOBoard = (Foundations, Columns, Reserves)

 type Foundations = [Card] -- only need to know top card
 
 type Columns = [[Card]]
 
 type Reserves = [Card]
-}

 ----------------------------------------------------------
 -- display an EOBoard
 displayEOB :: EOBoard -> IO String
 
 displayEOB (fnds,cols,res) = do
  let colStr = colsToString cols
  putStr "EOBoard\nFoundations  "
  putStrLn (show fnds)
  putStr  "Columns"
  putStr colStr
  putStr "\n\nReserve     "
  putStrLn (show res)
  putStr "\n---------------------------------------------\n"
  return ""

 colsToString :: Columns->String -- prepare String to print columns on separate lines
 
 colsToString cols =
  foldr (++) "" ["\n             "++(show col) |col<-cols]
  
-----------------------------------------------------------------------

-- display a list of EOBoards  

 displayEOBList :: [EOBoard]-> IO String
 
 displayEOBList eobl =  -- @ notation doesn't seem to work correctly
  do
   if (null eobl) then do (return "")
                  else do
                        displayEOB (head eobl)
                        displayEOBList (tail eobl)
 
   
-----------------------------------------------------------------

 --scoreBoard
 -- score is number of cards on foundations
 -- return a String for display
 
 scoreBoard :: EOBoard-> String 
 scoreBoard (fnds, cols, res) = "A LOSS: SCORE  " ++ (show (52- (length res) - (foldr (+) 0 (map length cols))))      

 -----------------------------------------------------------------------------
 -- play a game given initial board
 -- assuming a fn chooseMove :: EOBoard ->Maybe EOBoard
 -- & that toFoundations is handled outside
 
 displayEOGame :: EOBoard ->IO String
 
 displayEOGame b = do
  let (fnds,cols,res) = b -- apparently can't do this with @
  if ((null cols)&&(null res)) -- if cols & reserve empty its a win
     then return "A WIN"
     else 
      do
       displayEOB b -- display given board
       let res = chooseMove b
       if (isJust res) then
               do
                let nb = resMaybe res
                displayEOGame nb
              else
               do
                 let score = scoreBoard b
                 return score
 
 ------------------------------------------------  
 -- Maybe helper                
 resMaybe :: (Maybe a) -> a
 resMaybe (Just x) = x 

 -- MAIN FUNCTIONS USED FOR THE AUTO SOLITAIRE PLAYER
 ------------------------------------------------
 -- Runs chooseMove until the game ends and show the score
 eOGame :: EOBoard -> Int
 eOGame board@(_, columns, reserve)
     | null columns && null reserve = 52
     -- when moves are finished
     | isNothing move =  52 - sum (map length columns) - length reserve
     | otherwise = eOGame (fromJust move)
        where move = chooseMove board

 -- Play 100 games with 100 random seeds
 playEOGames :: Int -> [Int]
 playEOGames seed = map (eOGame.eODeal) (take 100 $ randoms (mkStdGen seed)::[Int])

 -- Finds the average score and the number of wins for a list of scores
 eOExpt :: Int -> (Int,Float)
 eOExpt seed = 
     let score = playEOGames seed
     -- show score of how many games are won and the percentage
     in (length(filter (==52) score), (fromIntegral.sum) score / (fromIntegral.length) score)

 -- FUNCTIONS USED FOR CHOOSING THE MOVES/ FINDING THE MOVES,
 -- THAT CAN BE USED FOR THE MAIN FUNCTIONS OF THE AUTO SOLITAIRE PLAYER
 ------------------------------------------------

 -- all possible moves from a board are rest-to-col moves, col-to-col moves
 -- and col-to-res moves
 findMoves :: EOBoard -> [EOBoard]
 findMoves board = toColumnsMoves board ++ cardToReserve board

 -- Gets all possible (res & col) single card to column moves
 toColumnsMoves :: EOBoard -> [EOBoard]
 toColumnsMoves board = 
      -- heads of all the cols and the cards in the reserve
      let possibleCards = getCards board
          moveBoard = cardToColumns board
      in  foldl (\c l -> if moveBoard l /= board then moveBoard l:c else c) [] possibleCards

 cardToColumns :: EOBoard -> Card -> EOBoard
 cardToColumns board@(foundations, columns, reserve) card
     | length columns < 8 && cardIsKing card = (foundations, [card]:foldl (getOtherColumns card) [] cols, filter (/= card) reserve)
     | not (cardIsKing card || null moveColumns) = (foundations, (card:head moveColumns):foldl (getOtherColumns card) [] cols, filter (/= card) reserve)
     | otherwise = board -- Card cannot be moved to any column
     where
         cols = filter (not.null) columns
         moveColumns = filter (\(l:ls) -> sCard card == l) cols
         getOtherColumns _ c [] = c
         getOtherColumns card c (l:ls)
             -- removes card from original column
             | l == card = ls:c 
             | cardIsKing card || l /= sCard card = (l:ls):c
             | otherwise = c

-- Gets all possible column head to reserve moves
 cardToReserve :: EOBoard -> [EOBoard]
 cardToReserve (foundations, columns, reserve)
     | length reserve < 8 = foldl (\c card -> (foundations, [if l == card then ls else l:ls | (l:ls) <- columns], card:reserve):c) [] (map head . filter (not.null) $ columns)
     | otherwise = []

 -- Gets all possible single moves for a board after moving to foundations
 -- and choose the best move possible in the case
 chooseMove :: EOBoard -> Maybe EOBoard
 chooseMove (_, [], []) = Nothing
 chooseMove board
    | boardScore /= checkBoard (fst bestMove) && boardScore /= snd bestNextMove = Just (goToFoundations (fst bestMove))
    | otherwise = Nothing
    where
          bestMove = lookOverMoves 3 board
          bestNextMove = lookOverMoves 1 (fst bestMove)
          boardScore = checkBoard board


-- FUNCTIONS USED FOR chooseMove
--------------------------------------------------------

-- best board with highest score
 getBestBoard :: EOBoard -> [(EOBoard, Int)] -> (EOBoard, Int)
 getBestBoard board boards
     -- gets board with the highest score or return the original board
     | not (null boards) = maximumBy (comparing snd) boards
     | otherwise = (board, -100) 

 -- takes a board and returns an integer of how good the board is
 checkBoard :: EOBoard -> Int
 checkBoard board@(foundations,columns,reserve) = (toFoundationsCount*35) + (foundationsCount*25) + (reserveCount*(-4) + (inOrderColumns*30))
     where
         toFoundationsCount = length (getPossibleCards board)
         foundationsCount = sum (map length foundations)
         inOrderColumns = orderingColumns columns
         reserveCount = length reserve

 -- counts how many columns ar in order from the bottom to the top
 orderingColumns :: Columns -> Int
 orderingColumns = foldl (checkInOrder 0) 0
     where
         checkInOrder _ card [] = card
         checkInOrder count card (l:ls)
             -- checks if cards are predecessors of the card below it
             -- and the column is based with any king
             | not (null ls || cardIsKing l) && sCard l == head ls = checkInOrder (count+1) card ls
             | cardIsKing l && null ls = card + count + 1
             | otherwise = card

 -- look in depth n and chooses the best move
 lookOverMoves :: Int -> EOBoard -> (EOBoard, Int)
 lookOverMoves 1 board = 
     let possibleBoards = findMoves.goToFoundations $ board
     in  getBestBoard board (map (\b -> (b, checkBoard b)) possibleBoards)
 lookOverMoves depth board = 
     -- recursively do all the possible baords from current position
     let bestNextLevel = foldl (\c b -> lookOverMoves (depth-1) b:c) [] (findMoves.goToFoundations $ board)
     in  getBestBoard board bestNextLevel
---------------------------------------------------

-- testing
 b1 = eODeal 23 --wining board
 b2 = eODeal 5  --lossing board