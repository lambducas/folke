module Main (
    main
) where

import Example (example)
import qualified Frontend.Main as Frontend
import Control.Concurrent (forkIO)

main :: IO ()
main = do
    _ <- forkIO Frontend.main
    example
