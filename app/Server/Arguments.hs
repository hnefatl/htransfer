{-# LANGUAGE LambdaCase #-}

module Arguments
(
    ResourceName,
    Resources,
    ParseError(..),
    parseArgs
) where

import qualified Data.Map as M
import Data.Foldable (traverse_)

import Data.List.Split (splitOn)

import Control.Monad.State
import Control.Monad.Catch

import Shared

data ParseError = InvalidFormat String
                | DuplicateResourceName ResourceName
instance Show ParseError where
    show (InvalidFormat s) = "Invalid argument format: " ++ show s
    show (DuplicateResourceName s) = "Duplicate resource name: " ++ show s
instance Exception ParseError


parseArgs :: MonadThrow m => [String] -> m Resources
parseArgs args = execStateT (traverse_ buildResourceLocations args) M.empty

buildResourceLocations :: MonadThrow m => String -> StateT Resources m ()
buildResourceLocations arg = do
            (name, path) <- lift $ formatArgs arg
            nameExists <- gets (M.member name)
            when nameExists (throwM $ DuplicateResourceName name)
            modify (M.insert name path)

formatArgs :: MonadThrow m => String -> m (ResourceName, FilePath)
formatArgs s = case splitOn ":" s of
                    [name, path] -> return (name, path)
                    _            -> throwM $ InvalidFormat s