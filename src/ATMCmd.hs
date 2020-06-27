{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Werror=incomplete-patterns #-}
{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors #-}

module ATMCmd where

import Control.Monad
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

data ATMCmd a (s :: ATMState) (s' :: ATMState) :: Type where
  InsertCard :: ATMCmd () Ready CardInserted
  EjectCard :: ATMCmd () s Ready
  GetPIN :: ATMCmd PIN CardInserted CardInserted
  CheckPIN :: PIN -> ATMCmd PINCheck CardInserted CardInserted
  StartSession :: Sing (p :: PINCheck) -> ATMCmd () CardInserted (PinCheckToState p)
  GetAmount :: ATMCmd Int s s
  Dispense :: Int -> ATMCmd () Session Session
  Message :: String -> ATMCmd () s s
  Return :: a -> ATMCmd a s s
  (:>>=) :: ATMCmd a s1 s2 -> (a -> ATMCmd b s2 s3) -> ATMCmd b s1 s3

infixl 2 :>>=, >>:

(>>:) :: ATMCmd () s1 s2 -> ATMCmd b s2 s3 -> ATMCmd b s1 s3
c1 >>: c2 = c1 :>>= const c2

atm :: ATMCmd () Ready Ready
atm =
  InsertCard
    >>: Message "Hello"
    >>: GetPIN
    :>>= CheckPIN
    :>>= \(FromSing ok) ->
      StartSession ok
        >>: case ok of
          SCorrectPIN ->
            GetAmount
              :>>= Dispense
              >>: EjectCard
              >>: Message "Remove card and cash"
          SWrongPIN ->
            EjectCard
              >>: Message "Incorrect PIN"

runATM :: ATMCmd a s1 s2 -> IO a
runATM InsertCard = putStrLn "Insert card and press enter" >> void getLine
runATM EjectCard = putStrLn "Card ejected"
runATM GetPIN = putStrLn "Enter pin:" >> getLine
runATM (CheckPIN pin) = return $ if pin == "1234" then CorrectPIN else WrongPIN
runATM (StartSession _) = return ()
runATM GetAmount = read <$> (putStrLn "Enter amount:" >> getLine) :: IO Int
runATM (Dispense cash) = putStrLn $ "Here is " ++ show cash
runATM (Message msg) = putStrLn msg
runATM (Return res) = return res
runATM (c :>>= f) = runATM c >>= \x -> runATM (f x)
