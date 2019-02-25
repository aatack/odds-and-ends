module Fractory.Item where

import qualified Data.Set as Set

data Crafter = Hand | Assembler Int | Furnace Int
data Recipe = Recipe Rational Rational [(Rational, String)] (Set.Set Crafter)

data Item = Raw | Crafted Recipe
