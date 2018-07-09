module Test where

import Data.Char
import Cesar

prop_neg_shift :: Int -> Char -> Bool
prop_neg_shift n c
    | shift ((-1)*n) (shift n c) == c = True
    | otherwise                       = False

prop_enc_length :: Int -> String -> Bool
prop_enc_length n cs
    | length (encode n cs) == length cs = True
    | otherwise                         = False
