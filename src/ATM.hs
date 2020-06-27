{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RebindableSyntax #-}
{-# OPTIONS_GHC -Werror=incomplete-patterns #-}
{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module ATM where

import ATMCmd
import Data.Singletons
import Prelude hiding ((>>), (>>=), fail)

atm' :: ATMCmd () Ready Ready
atm' = do
  InsertCard
  Message "Hello"
  pin <- GetPIN
  FromSing ok <- CheckPIN pin
  StartSession ok
  case ok of
    SCorrectPIN -> do
      amount <- GetAmount
      Dispense amount
      EjectCard
      Message "Remove card and cash"
    SWrongPIN -> do
      EjectCard
      Message "Incorrect PIN"

infixl 2 >>=, >>

(>>) :: ATMCmd () s1 s2 -> ATMCmd b s2 s3 -> ATMCmd b s1 s3
(>>) = (>>:)

(>>=) :: ATMCmd a s1 s2 -> (a -> ATMCmd b s2 s3) -> ATMCmd b s1 s3
(>>=) = (:>>=)

fail :: String -> ATMCmd a s s'
fail = error
