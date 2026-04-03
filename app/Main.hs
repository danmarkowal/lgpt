module Main where

import Control.Monad.State (StateT (runStateT))
import LGPT.Helpers
import LGPT.TUI qualified as TUI

-- | This is what gets run when you run the program.
--
--    It just calls the runREPL function in TUI.hs, which is where the real work
--    happens :)
main :: IO ()
main = do
  -- Pre-initialisation to set up the terminal
  runStart

  -- Actually run the loop!
  runStateT TUI.runREPL emptyState
  return ()