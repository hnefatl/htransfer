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
import Data.Maybe (isJust)
import Data.List.Split (splitOn)

import Control.Monad.State
import Control.Monad.Except

import Shared

data ParseError = InvalidFormat String
                | DuplicateResourceName ResourceName

instance Show ParseError where
    show (InvalidFormat s) = "Invalid argument format: " ++ show s
    show (DuplicateResourceName s) = "Duplicate resource name: " ++ show s

parseArgs :: [String] -> Either ParseError Resources
parseArgs args = runExcept $ execStateT (traverse_ buildResourceLocations args) M.empty

buildResourceLocations :: String -> StateT Resources (Except ParseError) ()
buildResourceLocations arg =
        case splitOn ":" arg of
            [name, path] -> do
                                nameExists <- gets (M.lookup name)
                                when (isJust nameExists) (throwError $ DuplicateResourceName name)
                                modify (M.insert name path)
            _            -> throwError $ InvalidFormat arg