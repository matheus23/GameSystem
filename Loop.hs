module Loop where

type IOStateProducer s = (s -> IO (Bool, s))

ioLoop :: IOStateProducer s -> s -> IO s
ioLoop loopFunc initialState = do
  (shouldContinue, nextIn) <- loopFunc initialState
  if shouldContinue
    then ioLoop loopFunc nextIn
    else return nextIn

recLoop :: IOStateProducer s -> s -> IO (Bool, s)
recLoop advanceWith oldState = do advanceWith oldState
