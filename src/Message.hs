{-# LANGUAGE DeriveGeneric #-}

module Message where

import qualified Data.ByteString.Lazy as BL
import Data.Serialize

import GHC.Generics

import Shared

data Message = ResourceListRequest
             | ResourceListResponse [ResourceName]
             | ResourceRequest ResourceName
             | ResourceResponse BL.ByteString
             deriving (Generic)
instance Serialize Message