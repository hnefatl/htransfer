module Shared
(
    ResourceName,
    Resources
) where

import qualified Data.Map as M

type ResourceName = String
type Resources = M.Map ResourceName FilePath