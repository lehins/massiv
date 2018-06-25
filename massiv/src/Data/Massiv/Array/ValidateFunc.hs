module Data.Massiv.Array.ValidateFunc where

import Data.Massiv.Core.Common
import Data.Validity

validateFunc :: (Index ix, Validity e) => ix -> ix -> (ix -> e) -> Validation
validateFunc initial sz arr =
    iter initial sz 1 (<) valid $ \ix validation ->
        validate (arr ix) `mappend` validation
