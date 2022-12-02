{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors #-}

module ATMCmd where

import Control.Monad (void)
import Control.XFreer
import Data.Kind
import Data.Singletons ()
import Data.Singletons.TH

$( singletons
     [d|
       data ATMState = Ready | CardInserted | Session
         deriving (Show)

       data PINCheck = CorrectPIN | WrongPIN
         deriving (Show)

       pinCheckToState :: PINCheck -> ATMState
       pinCheckToState = \case
         CorrectPIN -> Session
         WrongPIN -> CardInserted
       |]
 )

type PIN = String

type family HasCard (s :: ATMState) :: Constraint where
  HasCard CardInserted = ()
  HasCard Session = ()

data ATMCommand (s :: ATMState) (s' :: ATMState) a :: Type where
  InsertCard :: ATMCommand Ready CardInserted ()
  EjectCard :: HasCard s => ATMCommand s Ready ()
  GetPIN :: ATMCommand CardInserted CardInserted PIN
  CheckPIN :: PIN -> ATMCommand CardInserted CardInserted PINCheck
  StartSession :: SPINCheck p -> ATMCommand CardInserted (PinCheckToState p) ()
  GetAmount :: HasCard s => ATMCommand s s Int
  Dispense :: Int -> ATMCommand Session Session ()
  Message :: String -> ATMCommand s s ()

type ATMCmd = XFree ATMCommand

insertCard :: ATMCmd Ready CardInserted ()
insertCard = xfree InsertCard

ejectCard :: HasCard s => ATMCmd s Ready ()
ejectCard = xfree EjectCard

getPIN :: ATMCmd CardInserted CardInserted PIN
getPIN = xfree GetPIN

checkPIN :: PIN -> ATMCmd CardInserted CardInserted PINCheck
checkPIN = xfree . CheckPIN

startSession :: SPINCheck p -> ATMCmd CardInserted (PinCheckToState p) ()
startSession = xfree . StartSession

getAmount :: HasCard s => ATMCmd s s Int
getAmount = xfree GetAmount

dispense :: Int -> ATMCmd Session Session ()
dispense = xfree . Dispense

message :: String -> ATMCmd s s ()
message = xfree . Message

runATM :: ATMCmd s s' a -> IO a
runATM (Pure x) = return x
runATM (Bind c f) = runATMCmd c >>= \x -> runATM (f x)

runATMCmd :: ATMCommand s s' a -> IO a
runATMCmd InsertCard = putStrLn "Insert card (press enter)" >> void getLine
runATMCmd EjectCard = putStrLn "Card ejected"
runATMCmd GetPIN = putStrLn "Enter pin:" >> getLine
runATMCmd (CheckPIN pin) =
  if pin == "1234"
    then return CorrectPIN
    else return WrongPIN
runATMCmd (StartSession _) = return ()
runATMCmd GetAmount = read <$> (putStrLn "Enter amount:" >> getLine) :: IO Int
runATMCmd (Dispense cash) = putStrLn $ "Here is " ++ show cash
runATMCmd (Message msg) = putStrLn msg
