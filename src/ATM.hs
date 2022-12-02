{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE QualifiedDo #-}

module ATM where

import ATMCmd
import qualified Control.XMonad.Do as X
import Data.Singletons

atm :: ATMCmd Ready Ready ()
atm = X.do
  insertCard
  message "Hello"
  pin <- getPIN
  pinOK <- checkPIN pin
  case pinOK of
    FromSing ok -> X.do
      startSession ok
      case ok of
        SCorrectPIN -> X.do
          amount <- getAmount
          dispense amount -- this would fail to compile in SWrongPIN branch
          ejectCard
          message "Remove card and cash"
        SWrongPIN -> X.do
          message "Incorrect PIN"
          ejectCard
