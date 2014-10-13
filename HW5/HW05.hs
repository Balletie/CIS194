module HW05 where

import Ring
import Parser
import Data.Maybe    ( listToMaybe )

data Mod5 = MkMod Integer
  deriving (Read, Show, Eq)

instance Ring Mod5 where
  addId = MkMod 0
  addInv (MkMod a) = MkMod (5 - ( a `mod` 5))
  mulId = MkMod 1

  add (MkMod a) (MkMod b) = MkMod ( (a + b) `mod` 5 )
  mul (MkMod a) (MkMod b) = MkMod ( (a * b) `mod` 5 )

instance Parsable Mod5 where
  parse = (fmap toMod) . parseInt
      where
        parseInt = parse :: String -> Maybe(Integer, String)
        toMod (a,b) = (MkMod a, b)

-- | Use testMod5 to use all tests
testMod5 :: Bool
testMod5 = testMod5ParseLiteral   &&
           testMod5ParseAdd       &&
           testMod5ParseMul       &&
           testMod5ParseMulAndAdd &&
           testMod5AddInv

testMod5ParseLiteral :: Bool
testMod5ParseLiteral = parse "3" == Just(MkMod 3, "")

testMod5ParseAdd :: Bool
testMod5ParseAdd = parseRing "4 + 3" == Just(MkMod 2)

testMod5ParseMul :: Bool
testMod5ParseMul = parseRing "4 * 3" == Just(MkMod 2)

testMod5ParseMulAndAdd :: Bool
testMod5ParseMulAndAdd = parseRing "4 + 3 * 2" == Just(MkMod 0)

testMod5AddInv :: Bool
testMod5AddInv = addInv (MkMod 13) == (MkMod 2)
