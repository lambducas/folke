module Main where

import qualified Backend.MyLib (someFunc)

main :: IO ()
main = do
  putStrLn "Hello, Haskell!"
  Backend.MyLib.someFunc
