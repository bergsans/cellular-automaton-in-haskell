{-# LANGUAGE UnicodeSyntax #-}

module Main where

import           Lib
import           System.Environment
import           UI.NCurses

main ∷ IO ()
main = do
  args ← getArgs
  if not $ null args
    then do
      rawData ← readFile (head args)
      runCurses $ do
        setCursorMode CursorInvisible
        setEcho False
        w ← defaultWindow
        loop w (prepareData $ lines rawData) 0
    else putStrLn "./run <patters/filename>"

representation ∷ State → String
representation node
  | node == 1   = "██"
  | node == 2   = "▓▓"
  | node == 3   = "░░"
  | otherwise   = "  "

putNode ∷ Node → Update ()
putNode node
  | fst (fst node) == 0 = drawString $ "\n" ++ representation (snd node)
  | otherwise           = drawString $ representation (snd node)

loop ∷ Window → Game → Generation → Curses ()
loop window game generation = do
    updateWindow window $ do
      clear
      drawString "WireWorld\n"
      drawString "=====================\n"
      drawString $ "q: quit | Generation: " ++ show generation
      moveCursor 4 1
      sequence_ [putNode node | node ← game]
    render
    ev ← getEvent window (Just 200)
    case ev of
      Nothing → loop window (nextState game) (generation + 1)
      Just ev'
        | ev' == EventCharacter 'q' → return ()
        | otherwise → loop window (nextState game) (generation + 1)
