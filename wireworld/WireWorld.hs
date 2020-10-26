      {-# LANGUAGE UnicodeSyntax #-}

module Main where

import                  System.Environment
import                  Control.Concurrent
import                  Data.Maybe

type Point = (Int, Int)
type State = Integer
type Node  = (Point, State)
type Game  = [Node]

stateEMPTY     = 0
stateHEAD      = 1
stateTAIL      = 2
stateCONDUCTOR = 3

main ∷ IO ()
main = do
  rawData ← readFile "./demo"
  get (prepareData $ lines rawData)

representation ∷ State → String
representation node
  | node == stateHEAD      = "██"
  | node == stateTAIL      = "▓▓"
  | node == stateCONDUCTOR = "░░"
  | otherwise              = "  "

putNode ∷ Node → IO ()
putNode node
  | fst (fst node) == 0 = putStr $ "\n" ++ representation (snd node)
  | otherwise           = putStr $ representation (snd node)

clearScreen ∷ IO ()
clearScreen = putStr "\ESC[2J"

get ∷ Game → IO ()
get game = do
  sequence_ [putNode node | node ← game]
  clearScreen
  threadDelay 200000
  get (nextState game)

nodeState ∷ Char → Integer
nodeState char
  | char == '.' = stateEMPTY
  | char == 'h' = stateHEAD
  | char == '*' = stateCONDUCTOR
  | otherwise   = stateTAIL

makeRow ∷ String → Int → [Node]
makeRow row y =
  [((x,y), nodeState $ row !! x) | x ← [0..length row - 1]]

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
  | isNothing node                   = stateCONDUCTOR
  | snd (fromJust node) == stateHEAD = stateHEAD
  | otherwise                        = stateCONDUCTOR

checkNeighbors ∷ Node → Game → [Point] → Integer → Integer
checkNeighbors ((x,y), nodeState) game dirs isAny
  | isAny == stateHEAD = stateHEAD
  | null dirs          = stateCONDUCTOR
  | otherwise          = checkNeighbors
     ((x,y), nodeState)
     game
     (tail dirs)
     (isHead (getNode (x + fst (head dirs), y + snd (head dirs)) game))

nextNodeState ∷ Node → Game → State
nextNodeState ((x,y), state) game
  | state == stateHEAD      = stateTAIL
  | state == stateTAIL      = stateCONDUCTOR
  | state == stateCONDUCTOR = checkNeighbors ((x,y), state) game directions state
  | otherwise               = stateEMPTY

makeNode ∷ Node → Game → Node
makeNode node game = (fst node, nextNodeState node game)

nextState ∷ Game → Game
nextState game = map (`makeNode` game) game
