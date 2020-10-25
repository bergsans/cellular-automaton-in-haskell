{-# LANGUAGE UnicodeSyntax #-}

module Main where

import           System.Environment
import           Control.Concurrent
import           Data.Maybe

type Game            = [Cell]
type Cell            = (Point, Status)
type Point           = (Int, Int)
type Status          = Bool
type AliveNeighbours = Integer

prepareData ∷ [String] → Game
prepareData rawData =
  concat [ makeRow (rawData !! y) y | y ← [0..length rawData - 1]]

makeRow ∷ String → Int → [Cell]
makeRow row y =
  [((x,y), isCharLiving $ row !! x) | x ← [0..length row - 1]]

isCharLiving ∷ Char → Bool
isCharLiving char
  | char == 'o' = True
  | otherwise   = False

main ∷ IO ()
main = do
  rawData ← readFile "./pentadecathlon"
  get (prepareData $ lines rawData)

representation ∷ Status → String
representation cell
  | cell      = "[●]"
  | otherwise = "[∙]"

putCell ∷ Cell → IO ()
putCell cell
  | fst (fst cell) == 0 = putStr $ "\n" ++ representation (snd cell)
  | otherwise           = putStr $ representation (snd cell)

clearScreen ∷ IO ()
clearScreen = putStr "\ESC[2J"

get ∷ Game → IO ()
get game = do
  sequence_ [putCell cell | cell ← game]
  clearScreen
  threadDelay 200000
  get (nextState game)

nextState ∷ Game → Game
nextState game = map (`makeCell` game) game

makeCell ∷ Cell → Game → Cell
makeCell cell game = (
    fst cell,
    nextCellState (aliveNeighbours game cell directions 0) (snd cell)
  )

nextCellState ∷ AliveNeighbours → Status → Bool
nextCellState aliveNeighbours status
  | aliveNeighbours == 3 && not status = True
  | aliveNeighbours == 2 && status     = True
  | aliveNeighbours == 3 && status     = True
  | otherwise                          = False

aliveNeighbours ∷ Game → Cell → [Point] → AliveNeighbours → AliveNeighbours
aliveNeighbours game ((x,y), status) dirs count
  | null dirs    = count
  | isAlive game (x + fst (head dirs), y + snd (head dirs))
                 = aliveNeighbours game ((x,y), status) (tail dirs) (count + 1)
  | otherwise    = aliveNeighbours game ((x,y), status) (tail dirs) count

directions ∷ [Point]
directions = [(0,-1), (1,-1), (1,0), (1,1), (0,1), (-1,1), (-1,0), (-1,-1)]

isAlive ∷ Game → Point → Bool
isAlive game cell
  | isNothing(getCell cell game)      = False
  | snd (fromJust(getCell cell game)) = True
  | otherwise                         = False

getCell ∷ Point → Game → Maybe Cell
getCell pos [] = Nothing
getCell pos (((x,y), status) : rest)
  | pos == (x,y) = Just ((x,y), status)
  | otherwise = getCell pos rest
