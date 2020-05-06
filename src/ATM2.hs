{-# LANGUAGE TemplateHaskell #-}

module ATM2 where

import Data.Kind
import Control.Monad
import Data.Singletons()
-- import Data.Singletons.Prelude
import Data.Singletons.TH

$(singletons [d|
  data ATMCmd = CInsertCard
                | CEjectCard
                | CGetPIN
                | CCheckPIN 
                | CGetAmount
                | CDispense
                | CMessage
                | CReturn

  data ATMState = Ready | CardInserted | Session
    deriving (Show)

  data PINCheck = CorrectPIN | WrongPIN
    deriving (Show)  
  |])

$(singletons [d|
  cmdEndState :: ATMCmd -> a -> ATMState -> ATMState
  cmdEndState CInsertCard   _ Ready        = CardInserted
  cmdEndState CEjectCard    _ CardInserted = Ready
  cmdEndState CEjectCard    _ Session      = Ready
  cmdEndState CGetPIN       _ CardInserted = CardInserted
  -- does not compile:
  -- cmdEndState CCheckPIN pinOk CardInserted = case pinOk of
  --                                             CorrectPIN -> Session
  --                                             WrongPIN -> CardInserted
  cmdEndState CGetAmount    _ start        = start
  cmdEndState CDispense     _ Session      = Session
  cmdEndState CMessage      _ start        = start
  cmdEndState CReturn       _ start        = start
  -- cmdEndState _ _ _ = ??? -- there is incomplete pattern match, no error though...
  |])
  
type PIN = String

data Cmd :: forall k. k -> ATMState -> ATMState -> Type where
  InsertCard :: Cmd () 'Ready 'CardInserted
  EjectCard  :: Cmd () start 'Ready
  GetPIN     :: Cmd PIN 'CardInserted 'CardInserted
  CheckPin   :: PIN -> Cmd (a :: PINCheck) 'CardInserted 'Session
  GetAmount  :: Cmd Int state state
  Dispense   :: Int -> Cmd () 'Session 'Session
  Message    :: String -> Cmd () state state
  Return     :: a -> Cmd a state state
  (:>>)      :: Cmd a s1 s2 -> Cmd b s2 s3 -> Cmd b s1 s3
  -- (:>>=)     :: Cmd a s1 s2 -> (a -> Cmd b s2 s3) -> Cmd b s1 s3


atm :: Cmd () 'Ready 'Ready
atm = InsertCard
      :>> Message "Hello"
      :>> GetPIN
      -- :>>= \pin -> CheckPIN pin
      :>> GetAmount
      -- :>>= \cash -> Dispense cash
      :>> EjectCard
      :>> Message "Bye"

-- runATM :: Cmd a s1 s2 -> IO a
-- runATM InsertCard      = putStrLn "Insert card and print enter" >> void getLine
-- runATM EjectCard       = putStrLn "Card ejected"
-- runATM GetPIN          = putStrLn "Enter pin:" >> getLine
-- -- runATM (CheckPIN pin) = return $ if pin == "1234" then SCorrectPIN else SWrongPIN
-- runATM GetAmount       = read <$> (putStrLn "Enter amount:" >> getLine)
-- runATM (Dispense cash) = putStrLn $ "Here is " ++ show cash
-- runATM (Message msg)   = putStrLn msg
-- runATM (Return res)    = return res
-- runATM (c1 :>> c2)     = runATM c1 >> runATM c2
-- -- runATM (c :>>= f)      = runATM c >>= \x -> runATM (f x)
