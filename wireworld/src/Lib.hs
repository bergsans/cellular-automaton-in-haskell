{-# LANGUAGE UnicodeSyntax #-}

module Lib (
      Game
    , Type
    , Node
    , Point
    , prepareData
    , nextState
    , Generation
  ) where

type Point      = (Int, Int)
type Type       = Integer
type Node       = (Point, Type)
type Game       = [Node]
type Generation = Integer

--------------------------------------
-- Format pattern
--------------------------------------
isCharLiving ∷ Char → Integer
isCharLiving char
        | char == '.' = 0
        | char == 'h' = 1
        | char == '*' = 3
        | otherwise   = 2

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

getNode ∷ Eq a ⇒ a → [(a, b)] → [(a, b)]
getNode k = filter (\n → fst n == k)

isHead ∷ [Node] → Integer
isHead node
  | null node            = 3
  | snd (head node) == 1 = 1
  | otherwise            = 3

checkNeighbors ∷ Node → Game → [Point] → Integer → Integer
checkNeighbors ((x,y), nodeType) game dirs isAny
  | isAny == 1    = 1
  | null dirs     = 3
  | otherwise     =
       checkNeighbors
        ((x,y), nodeType)
        game
        (tail dirs)
        (isHead (getNode (x + fst (head dirs), y + snd (head dirs)) game))

nextNodeState ∷ Node → Game → Integer
nextNodeState ((x,y), nodeType) game
  | nodeType == 1 = 2
  | nodeType == 2 = 3
  | nodeType == 3 = checkNeighbors ((x,y), nodeType) game directions nodeType
  | otherwise = 0

makeNode ∷ Node → Game → Node
makeNode node game =
  (
    fst node,
    nextNodeState node game
  )

nextState ∷ Game → Game
nextState game = map (`makeNode` game) game

