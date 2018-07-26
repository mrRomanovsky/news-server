module Model where

class Model m where
  create :: m -> IO ()
  read :: IO [m]
  update :: m -> m -> IO ()
  delete :: m -> IO ()