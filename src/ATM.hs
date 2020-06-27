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

atm' :: ATMCmd Ready Ready ()
atm' = do
  InsertCard
  Message "Hello"
  pin <- GetPIN
  FromSing ok <- CheckPIN pin
  session ok
  atm'
  where
    session :: SPINCheck p -> ATMCmd CardInserted Ready ()
    session ok = do
      StartSession ok
      case ok of
        SCorrectPIN -> do
          amount <- GetAmount
          Dispense amount
          EjectCard
          Message "Remove card and cash"
        SWrongPIN -> do
          Message "Incorrect PIN"
          EjectCard

infixl 2 >>=, >>

(>>) :: ATMCmd s1 s2 () -> ATMCmd s2 s3 b -> ATMCmd s1 s3 b
(>>) = (:>>)

(>>=) :: ATMCmd s1 s2 a -> (a -> ATMCmd s2 s3 b) -> ATMCmd s1 s3 b
(>>=) = (:>>=)

fail :: String -> ATMCmd s s' a
fail = error
