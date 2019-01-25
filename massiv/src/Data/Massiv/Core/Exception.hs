module Data.Massiv.Core.Exception where


data IndexException ix =
  ZeroIndex ix
  --ZeroIndex (forall . Index ix => ix)
  -- ^ Index contains a zero value in along one of the dimensions:
