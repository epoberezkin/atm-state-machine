import Data.Vect

PIN : Type
PIN = Vect 4 Char

data ATMState = Ready | CardInserted | Session

data PINCheck = CorrectPIN | IncorrectPIN

data HasCard : ATMState -> Type where
     HasCI      : HasCard CardInserted
     HasSession : HasCard Session

data ATMCmd : (ty : Type) -> ATMState -> (ty -> ATMState) -> Type where
  InsertCard : ATMCmd ()  Ready        (const CardInserted)

  EjectCard  : {auto prf : HasCard state} ->  ATMCmd () state (const Ready)

  GetPIN     : ATMCmd PIN CardInserted (const CardInserted)

  CheckPIN : PIN -> ATMCmd PINCheck CardInserted
                        (\check => case check of
                                        CorrectPIN => Session
                                        IncorrectPIN => CardInserted)

  GetAmount : ATMCmd Nat state (const state)

  Dispense : (amount : Nat) -> ATMCmd () Session (const Session)

  Message  : String -> ATMCmd () state (const state)
  Pure  : (res : ty) -> ATMCmd ty (state_fn res) state_fn
  (>>=) : ATMCmd a state1 state2_fn
          -> ((res : a) -> ATMCmd b (state2_fn res) state3_fn)
          -> ATMCmd b state1 state3_fn


atm : ATMCmd () Ready (const Ready)
atm = do InsertCard
         pin <- GetPIN
         pinOK <- CheckPIN pin
         case pinOK of
              CorrectPIN => do cash <- GetAmount
                               Dispense cash
                               EjectCard
              IncorrectPIN => EjectCard

readVect : (n : Nat) -> IO (Vect n Char)
readVect Z = do discard <- getLine
                pure []
readVect (S k) = do ch <- getChar
                    chs <- readVect k
                    pure (ch :: chs)

readPIN : IO PIN
readPIN = readVect 4

testPIN : PIN
testPIN = ['1', '2', '3', '4']

runATM : ATMCmd res inState outState_fn -> IO res
runATM InsertCard = do putStrLn "Please insert your card (press enter)"
                       x <- getLine
                       pure ()

runATM EjectCard = putStrLn "Card ejected"

runATM GetPIN = do putStrLn "Enter PIN:"
                   readPIN

runATM (CheckPIN pin) = if pin == testPIN
                          then pure CorrectPIN
                          else pure IncorrectPIN

runATM GetAmount = do putStrLn "Enter amount:"
                      x <- getLine
                      pure (cast x)

runATM (Dispense amount) = putStrLn ("Here is " ++ show amount)

runATM (Message msg) = putStrLn msg

runATM (Pure res) = pure res

runATM (x >>= f) = do x' <- runATM x
                      runATM (f x')

main : IO ()
main = runATM atm
