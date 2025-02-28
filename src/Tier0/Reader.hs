module Tier0.Reader (Environment (..), EnvironmentM, formatUserName, formatHost, formatCurrentDir, formatPrompt) where

import Control.Monad.Reader

data Environment = Environment
  { username :: String
  , isSuperUser :: Bool
  , host :: String
  , currentDir :: String
  } deriving Eq

type EnvironmentM = Reader Environment

formatUserName :: EnvironmentM String
formatUserName = do
			(Environment uname isSU _ _) <- ask
			if (isSU) then return "root" else return uname
  
formatHost :: EnvironmentM String
formatHost = do
		(Environment _ _ h _) <- ask
		return h

formatCurrentDir :: EnvironmentM String
formatCurrentDir = do
			(Environment _ _ _ currDir) <- ask
			return currDir

formatPrompt :: EnvironmentM String
formatPrompt = do
		uname <- formatUserName
		h <- formatHost
		currDir <- formatCurrentDir
		return (uname ++ "@" ++ h ++ ":" ++ currDir ++ "$")
