module Overflow
( Host (..)
) where

import Data.Text

-- IP, PORT
data Host =
    Host Text Int
    deriving (Show)
