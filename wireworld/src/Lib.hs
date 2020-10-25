{-# LANGUAGE UnicodeSyntax #-}

module Lib (
      Game
    , State
    , Node
    , Point
    , prepareData
    , nextState
    , Generation
  ) where

import Data.Maybe

type Point      = (Int, Int)
type State      = Integer
type Node       = (Point, State)
type Game       = [Node]
type Generation = Integer

stateEMPTY     = 0
stateHEAD      = 1
stateTAIL      = 2
stateCONDUCTOR = 3

cellState ∷ Char → Integer
cellState char
  | char == '.' = stateEMPTY
  | char == 'h' = stateHEAD
  | char == '*' = stateCONDUCTOR
  | otherwise   = stateTAIL

makeRow ∷ String → Int → [Node]
makeRow row y =
  [((x,y), cellState $ row !! x) | x ← [0..length row - 1]]

prepareData ∷ [String] → Game
prepareData rawData =
  concat [ makeRow (rawData !! y) y | y ← [0..length rawData - 1]]

directions ∷ [Point]
directions = [(0,-1), (1,-1), (1,0), (1,1), (0,1), (-1,1), (-1,0), (-1,-1)]

getNode ∷ Point → Game → Maybe Node
getNode pos [] = Nothing
getNode pos (((x,y), status) : rest)
  | pos == (x,y) = Just ((x,y), status)
  | otherwise = getNode pos rest


isHead ∷ Maybe Node → Integer
isHead node
  | isNothing node           = stateCONDUCTOR
  | snd (fromJust node) == 1 = stateHEAD
  | otherwise                = stateCONDUCTOR

checkNeighbors ∷ Node → Game → [Point] → Integer → Integer
checkNeighbors ((x,y), nodeState) game dirs isAny
  | isAny == 1    = stateHEAD
  | null dirs     = stateCONDUCTOR
  | otherwise     =
      checkNeighbors
        ((x,y), nodeState)
        game
        (tail dirs)
        (isHead (getNode (x + fst (head dirs), y + snd (head dirs)) game))

nextNodeState ∷ Node → Game → State
nextNodeState ((x,y), nodeState) game
  | nodeState == 1 = stateTAIL
  | nodeState == 2 = stateCONDUCTOR
  | nodeState == 3 = checkNeighbors ((x,y), nodeState) game directions nodeState
  | otherwise      = stateEMPTY

makeNode ∷ Node → Game → Node
makeNode node game = (fst node, nextNodeState node game)

nextState ∷ Game → Game
nextState game = map (`makeNode` game) game
