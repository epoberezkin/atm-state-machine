{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
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

type family HasCard (s :: ATMState) :: Constraint where
  HasCard CardInserted = ()
  HasCard Session = ()

data ATMCmd (s :: ATMState) (s' :: ATMState) a :: Type where
  InsertCard :: ATMCmd Ready CardInserted ()
  EjectCard :: HasCard s => ATMCmd s Ready ()
  GetPIN :: ATMCmd CardInserted CardInserted PIN
  CheckPIN :: PIN -> ATMCmd CardInserted CardInserted PINCheck
  StartSession :: SPINCheck p -> ATMCmd CardInserted (PinCheckToState p) ()
  GetAmount :: HasCard s => ATMCmd s s Int
  Dispense :: Int -> ATMCmd Session Session ()
  Message :: String -> ATMCmd s s ()
  Pure :: a -> ATMCmd s s a
  (:>>=) :: ATMCmd s1 s2 a -> (a -> ATMCmd s2 s3 b) -> ATMCmd s1 s3 b
  (:>>) :: ATMCmd s1 s2 () -> ATMCmd s2 s3 b -> ATMCmd s1 s3 b

infixl 2 :>>=, :>>

atm :: ATMCmd Ready Ready ()
atm =
  InsertCard
    :>> Message "Hello"
    :>> GetPIN
    :>>= CheckPIN
    :>>= session
    :>> atm
  where
    session :: PINCheck -> ATMCmd CardInserted Ready ()
    session (FromSing ok) =
      StartSession ok
        :>> case ok of
          SCorrectPIN ->
            GetAmount
              :>>= Dispense
              :>> EjectCard
              :>> Message "Remove card and cash"
          SWrongPIN ->
            Message "Incorrect PIN"
              :>> EjectCard

runATM :: ATMCmd s1 s2 a -> IO a
runATM InsertCard = putStrLn "Insert card (press enter)" >> void getLine
runATM EjectCard = putStrLn "Card ejected"
runATM GetPIN = putStrLn "Enter pin:" >> getLine
runATM (CheckPIN pin) =
  if pin == "1234"
    then return CorrectPIN
    else return WrongPIN
runATM (StartSession _) = return ()
runATM GetAmount = read <$> (putStrLn "Enter amount:" >> getLine) :: IO Int
runATM (Dispense cash) = putStrLn $ "Here is " ++ show cash
runATM (Message msg) = putStrLn msg
runATM (Pure res) = return res
runATM (c :>>= f) = runATM c >>= \x -> runATM (f x)
runATM (c :>> c') = runATM c >> runATM c'

instance Functor (ATMCmd s s') where
  fmap :: (a -> b) -> ATMCmd s s' a -> ATMCmd s s' b
  fmap f c = c :>>= Pure . f

instance Applicative (ATMCmd s s) where
  pure :: a -> ATMCmd s s a
  pure = Pure
  (<*>) :: ATMCmd s s (a -> b) -> ATMCmd s s a -> ATMCmd s s b
  cf <*> c = cf :>>= \f -> c :>>= \x -> pure (f x)
