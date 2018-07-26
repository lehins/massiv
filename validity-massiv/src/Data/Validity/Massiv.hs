{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.Validity.Massiv where

import Import

import Data.Massiv.Array
import Data.Massiv.Core

import qualified Data.Primitive.Array as A
import qualified Data.Vector.Storable as SV
import qualified Data.Vector.Unboxed as UV

import qualified GHC.Exts as GHC (IsList(..))

instance Validity Comp

instance Validity Dim where
    validate (Dim n) = declare "A Dim is positive" $ n >= 0

instance Validity Ix0

--instance Validity (State s) where
--    validate = trivialValidation
instance Validity LN

instance Validity L

instance Validity (Elt LN ix e) => Validity (Array L ix e)

instance Validity (Elt LN ix e) => Validity (Array LN ix e)

instance (Generic e, Validity e) => Validity (Border e)

instance Validity Ix2

instance Validity (Ix (n - 1)) => Validity (IxN n)

instance Validity DI

--instance (Index ix, Validity ix, Validity e) => Validity (Array DI ix e)
instance Validity D

--instance (Index ix, Validity ix, Validity e) => Validity (Array D ix e) where
--    validate arr =
--        mconcat
--            [ delve "comp" $ getComp arr
--            , delve "size" $ getSize arr
--            , decorate "array" $ validateFunc (pureIndex 0) dSize dUnsafeIndex
--            ]
instance Validity DW

--instance (Num ix, Index ix, Validity ix, Validity (Array D ix e), Validity e) =>
--         Validity (Array DW ix e) where
--    validate DWArray {..} =
--        mconcat
--            [ delve "Array D" wdArray
--            , delve "stencil size" wdStencilSize
--            , delve "window start index" wdWindowStartIndex
--            , delve "window size" wdWindowSize
--            , declare "window start index must be positive" $
--              wdWindowStartInde x >= pureIndex 0
--            , declare "window fits in array" $
--              wdWindowStartIndex + wdWindowSiz e <= size wdArray
--            , validateFunc
--                  wdWindowStartIndex
--                  (wdWindowStartIndex + wdWindowSiz e)
--                  wdWindowUnsafeIndex
--            ]
instance Validity P

--instance (Validity ix, Index ix) => Validity (Array P ix e) where
--    validate arr =
--        delve "comp" (getComp arr) `mappend` delve "size" (getSize arr)
--
instance (Validity ix, Index ix) => Validity (Array P ix e) where
    validate arr =
        delve "comp" (getComp arr) <> delve "size" (getSize arr) <>
        delve "data" (toByteArray arr)

instance Validity N
#if MIN_VERSION_primitive(0,6,2)
instance Validity a => Validity (A.Array a) where
    validate = foldMap validate
#else
instance Validity a => Validity (A.Array a) where
    validate = trivialValidation
#endif
instance (Validity ix, Index ix, Validity e) => Validity (Array N ix e)

instance Validity B

instance (Validity ix, Index ix, Validity e) => Validity (Array B ix e)

instance Validity M

instance (Validity ix, Index ix, Validity e) => Validity (Array M ix e) where
    validate arr =
        mconcat
            [ delve "comp" $ getComp arr
            , delve "size" $ getSize arr
            , decorate "Array M elements" $
              checkUnsafeLinearIndex (totalElem m Size) mUnsafeLinearIndex
            ]

checkUnsafeLinearIndex :: Validity e => Int -> (Int -> e) -> Validation
checkUnsafeLinearIndex n func = foldMap (validate . func) $ [0 .. n - 1]

instance Validity S

instance (SV.Storable e, Validity e) => Validity (SV.Vector e) where
    validate = declare "elements" . foldMap validate . GHC.toList

instance (SV.Storable e, Validity ix, Index ix, Validity e) =>
         Validity (Array S ix e)

instance Validity U

instance (UV.Unbox e, Validity e) => Validity (UV.Vector e) where
    validate =
        declare "elements" .
        UV.foldl' (\val x -> val `mappend` validate x) mempty

instance (UV.Unbox e, Validity ix, Index ix, Validity e) =>
         Validity (Array U ix e)

validateFunc :: (Index ix, Validity e) => ix -> ix -> (ix -> e) -> Validation
validateFunc initial sz arr =
    iter initial sz 1 (<) valid $ \ix validation ->
        validate (arr ix) `mappend` validation

--instance (Validity ix, Index ix) => Validity (Stencil ix e a) where
--    validate Stencil {..} =
--        mconcat
--            [ delve "size" stencilSize
--            , delve "center" stencilCenter
--            , declare "center < size" $ stencilCenter < stencilSize
--            ]
instance Validity e => Validity (Value e)
