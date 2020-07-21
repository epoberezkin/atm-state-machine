{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RebindableSyntax #-}
{-# OPTIONS_GHC -Werror=incomplete-patterns #-}
{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module ATM where

import ATMCmd
import Control.XMonad.Do
import Data.Singletons
import Prelude hiding ((>>), (>>=))

atm' :: ATMCmd Ready Ready ()
atm' = do
  insertCard
  message "Hello"
  pin <- getPIN
  pinOK <- checkPIN pin
  case pinOK of
    FromSing ok -> do
      startSession ok
      case ok of
        SCorrectPIN -> do
          amount <- getAmount
          dispense amount -- this would fail to compile in SWrongPIN branch
          ejectCard
          message "Remove card and cash"
        SWrongPIN -> do
          message "Incorrect PIN"
          ejectCard
