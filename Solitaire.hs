module Solitaire where

    import System.Random
    import Data.List
    import Data.Maybe
    import Data.Function
    import Data.Ord
    import Debug.Trace

    --datatype for Suits
    data Suit = Diamonds|Hearts|Spades|Clubs 
        deriving (Eq,Show,Enum)
    
    --datatype for Pips
    data Pip = Ace|Two|Three|Four|Five|Six|Seven|Eight|Nine|Ten|Jack|Queen|King
        deriving (Eq,Show,Enum,Ord)
    
    --dataype for Cards
    type Card = (Suit,Pip)
    
    --dataype for Deck
    type Deck = [Card]
    
    --datatypes for EOBoard elements
    type Reserve = [Card]
    type Columns = [[Card]]
    type Foundations = [[Card]]

    --datatype for EOBoard
    type EOBoard = (Foundations,Columns,Reserve)

    --pack to show all 52 cards
    pack :: Deck
    pack = [(suit,pip) | suit <- [(Diamonds)..(Clubs)], pip <- [(Ace)..(King)]]

    --sCard - takes a card and return its successor
    sCard :: Card -> Card
    sCard (suit,pip) = (suit, succ pip)

    --pCard - takes a card and return its predeccessor
    pCard :: Card -> Card
    pCard (suit,pip) = (suit, pred pip)
    
    --verify if the card is a King
    cardIsKing :: Card -> Bool
    cardIsKing (suit,pip)
        |pip == King = True
        |otherwise = False

    --verify if the card is an Ace
    cardIsAce :: Card -> Bool
    cardIsAce (suit,pip)
        |pip == Ace = True
        |otherwise = False
   
    --shuffle a pack
    shuffle :: Int -> Deck 
    shuffle seed = map fst ( sortBy (\(_,d1) (_,d2) -> compare d1 d2) 
                  (zip pack dlis))
                  where dlis = take 52 (randoms (mkStdGen seed) :: [Int])
    
    -- give a seed that will shuffle a pack and introduce it in the board
    eODeal :: Int-> EOBoard
    eODeal seed = 
        let
        -- shuffle the pack with e given seed
        shufflePack = shuffle seed
        foundations = []
        -- shuffle the cards from the pack to put them in the columns
        columns = [(take 6 shufflePack), (take 6 (drop 6 shufflePack)),  
                 (take 6 (drop 12 shufflePack)), (take 6 (drop 18 shufflePack)), 
                 (take 6 (drop 24 shufflePack)), (take 6 (drop 30 shufflePack)), 
                 (take 6 (drop 36 shufflePack)), (take 6 (drop 42 shufflePack))]
        reserves = drop 48 shufflePack
        -- put the pack shuffled in the board
        in
        (foundations,columns,reserves) 
 
    -- Get top card from columns and all cards from reserve
    getCards :: EOBoard -> [Card]
    getCards (foundations, columns, reserve) = 
        let addToHead cards [] = cards
            addToHead cards (l:ls) = l:cards
        in  foldl addToHead [] columns ++ reserve

    -- Get some successors to put in foundations 
    getSuccsFoundations :: Foundations -> [Card]
    getSuccsFoundations = foldl getSuccsFoundations []
        where
           getSuccsFoundations card (l:ls)
               | cardIsKing l = card
               | otherwise = sCard l:card 

    -- Top cards that are aces or successors to foundations
    getPossibleCards :: EOBoard -> [Card]
    getPossibleCards board@(foundations,columns,reserve) = filter (\l -> cardIsAce l || l `elem` succsFoundations) cards
         where
             cards = getCards board
             succsFoundations = getSuccsFoundations foundations

    -- Takes a board and a card - moves the card from the top of the column
    -- or from the reserve to the top of the correct foundation
    makeFoundationMove :: EOBoard -> Card -> EOBoard
    makeFoundationMove (foundations, columns, reserve) card@(_,pip)
        -- If the card is an ace, then just add it to foundations as it won't work with map
        | pip == Ace = ([card]:foundations, filter (not.null) (foldl (updateCols card) [] columns), filter (/= card) reserve)
        | otherwise = (map (updateFounds card) foundations, filter (not.null) (foldl (updateCols card) [] columns), filter (/= card) reserve)
        where
            -- puts the card on the top of the right foundation
            updateFounds card (l:ls)
                | cardIsKing l = l:ls
                | card == sCard l = card:l:ls
                | otherwise = l:ls
            -- removes the card from the head of the column
            updateCols card c [] = c
            updateCols card c (l:ls)
                | card == l = ls:c
                | otherwise = (l:ls):c

    -- Makes possible moves (all) to the foundations
    goToFoundations :: EOBoard -> EOBoard
    goToFoundations board@(foundations, columns, reserve)
        -- Return the board
        | null movableCards = board
        -- Call recursively the function  with the board after making possible moves
        | otherwise = goToFoundations $ foldl makeFoundationMove board movableCards
        where
            movableCards = getPossibleCards board 
    