module Tier1.Reader (cd, su) where

import Control.Monad.Reader
import Tier0.Reader (Environment (..), EnvironmentM)

cd :: String -> EnvironmentM a -> EnvironmentM a
cd dir env = do
		(Environment uname isSU h currDir) <- ask
		local (const (Environment uname isSU h (currDir ++ "/" ++ dir))) env
		

su :: EnvironmentM a -> EnvironmentM a
su env = do
		(Environment uname _ h currDir) <- ask
		local (const (Environment uname True h currDir)) env

