{-# LANGUAGE UnicodeSyntax #-}

module Lib (
      Game
    , Status
    , Node
    , Point
    , prepareData
    , nextState
    , Generation
  ) where

import               Data.Maybe

type Point      = (Int, Int)
type Status     = Bool
type Node       = (Point, Status)
type Game       = [Node]
type Generation = Integer

--------------------------------------
-- Format pattern
--------------------------------------
isCharLiving ∷ Char → Bool
isCharLiving char
  | char == 'o' = True
  | otherwise   = False

makeRow ∷ String → Int → [Node]
makeRow row y =
      [((x,y), isCharLiving $ row !! x) | x ← [0..length row - 1]]

prepareData ∷ [String] → Game
prepareData rawData =
      concat [ makeRow (rawData !! y) y | y ← [0..length rawData - 1]]

--------------------------------------
-- Logic
--------------------------------------
directions ∷ [Point]
directions = [(0,-1), (1,-1), (1,0), (1,1), (0,1), (-1,1), (-1,0), (-1,-1)]

isAlive ∷ Game → Point → Bool
isAlive game node
  | isNothing(getCell node game)      = False
  | snd (fromJust(getCell node game)) = True
  | otherwise                         = False

getCell ∷ Point → Game → Maybe Node
getCell pos [] = Nothing
getCell pos (((x,y), status) : rest)
  | pos == (x,y) = Just ((x,y), status)
  | otherwise = getCell pos rest

nextNodeState ∷ Integer → Bool → Bool
nextNodeState aliveNeighbours status
  | aliveNeighbours == 3 && not status = True
  | aliveNeighbours == 2 && status     = True
  | aliveNeighbours == 3 && status     = True
  | otherwise                          = False

makeNode ∷ Node → Game → Node
makeNode node game =
  (
    fst node,
    nextNodeState (aliveNeighbours game node directions 0) (snd node)
  )

nextState ∷ Game → Game
nextState game = map (`makeNode` game) game

aliveNeighbours ∷ Game → Node → [Point] → Integer → Integer
aliveNeighbours game ((x,y), status) dirs count
  | null dirs         = count
  | isAlive game (x + fst (head dirs), y + snd (head dirs))
                      = aliveNeighbours game ((x,y), status) (tail dirs) (count + 1)
  | otherwise         = aliveNeighbours game ((x,y), status) (tail dirs) count
