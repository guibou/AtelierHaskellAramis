{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}

import GHC.Generics
import Options.Generic

data GitOperation =
  Checkout {branch :: String, flag :: Bool}
  | Commit {message :: Maybe String}
  | Move {from :: FilePath, to :: FilePath}
  deriving (Generic, Show, ParseRecord)

main = do
  op <- getRecord "This program is a simplified GIT interface"

  print (op :: GitOperation)
