module ATM1 where

import Data.Kind
import Control.Monad

data ATMState = Ready | CardInserted | Session
  deriving (Show)

data PINCheck = CorrectPIN | WrongPIN
  deriving (Show)
  
type PIN = String

data ATMCmd (a :: Type) (start :: ATMState) (end :: ATMState) :: Type where
  InsertCard :: ATMCmd () 'Ready 'CardInserted
  EjectCard  :: ATMCmd () state  'Ready
  GetPIN     :: ATMCmd PIN 'CardInserted 'CardInserted
  CheckPIN   :: PIN -> ATMCmd PINCheck 'CardInserted 'Session
  GetAmount  :: ATMCmd Int state state
  Dispense   :: Int -> ATMCmd () 'Session 'Session
  Message    :: String -> ATMCmd () state state
  Return     :: a -> ATMCmd a state state
  (:>>)      :: ATMCmd a s1 s2 -> ATMCmd b s2 s3 -> ATMCmd b s1 s3
  (:>>=)     :: ATMCmd a s1 s2 -> (a -> ATMCmd b s2 s3) -> ATMCmd b s1 s3

atm :: ATMCmd () 'Ready 'Ready
atm = InsertCard
      :>> Message "Hello"
      :>> GetPIN
      :>>= \pin -> CheckPIN pin
      :>> GetAmount
      :>>= \cash -> Dispense cash
      :>> EjectCard
      :>> Message "Bye"
      :>> Return ()

runATM :: ATMCmd res start end -> IO res
runATM InsertCard      = putStrLn "Insert card and print enter" >> void getLine
runATM EjectCard       = putStrLn "Card ejected"
runATM GetPIN          = putStrLn "Enter pin:" >> getLine
runATM (CheckPIN pin)  = return $ if pin == "1234" then CorrectPIN else WrongPIN
runATM GetAmount       = read <$> (putStrLn "Enter amount:" >> getLine)
runATM (Dispense cash) = putStrLn $ "Here is " ++ show cash
runATM (Message msg)   = putStrLn msg
runATM (Return res)    = return res
runATM (cmd1 :>> cmd2) = runATM cmd1 >> runATM cmd2
runATM (cmd1 :>>= f)   = runATM cmd1 >>= \x -> runATM (f x)
