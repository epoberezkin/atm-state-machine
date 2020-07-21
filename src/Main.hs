module Main where

import ATM (atm')
import ATMCmd (runATM)
import Control.Monad (forever)

main :: IO ()
main = runATM (forever atm')
