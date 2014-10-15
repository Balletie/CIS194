module HW05 where

import Ring
import Parser
import Data.Maybe    ( listToMaybe )
import Data.Char

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

-- | Test everything
testAll = testMod5 &&
          testMat2x2 &&
          testBool

-- | Use testMod5 to use all tests for Mod5
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

data Mat2x2 = Mat Integer Integer Integer Integer
  deriving (Read, Eq)

instance Show Mat2x2 where
  show (Mat a b c d) = "[[" ++ show a ++ "," ++ show b ++ "]["
                            ++ show c ++ "," ++ show d ++ "]]"

instance Ring Mat2x2 where
  addId = Mat 0 0 0 0
  addInv (Mat a b c d) = Mat (-a) (-b) (-c) (-d)
  mulId = Mat 1 0 0 1

  add (Mat a b c d)
      (Mat x y z w) = Mat (a+x) (b+y) (c+z) (d+w)

  mul (Mat a b c d)
      (Mat x y z w) = Mat (a*x + b*z) (a*y + b*w) (c*x + d*z) (c*y + d*w)

instance Parsable Mat2x2 where
  parse x = (eatInts x) >>= makeMat
            where makeMat (x:y:z:w:[], string) = Just ((Mat x y z w), string)
                  makeMat _                    = Nothing

-- |Simple implementation: just "eat" integers until we reach "]]"
-- Don't use spaces inside the matrix (especially not before the "]]")
eatInts :: String -> Maybe ([Integer], String)
eatInts (']':']':xs) = Just ([], xs)
eatInts x            = (readInt x) >>= (addToMaybe)
    where addToMaybe (int, string) = eatInts string >>= addToList int
          addToList int (xs,str) = Just(int:xs, str)

readInt :: String -> Maybe (Integer, String)
readInt = listToMaybe . reads . dropWhile (not . isDigit)

-- | Use testMat2x2 to use all tests for Mat2x2
testMat2x2 = testMat2x2ParseLiteral &&
             testMat2x2ParseAdd &&
             testMat2x2ParseMul &&
             testMat2x2ParseMulNonComm &&
             testMat2x2AddInv

testMat2x2ParseLiteral :: Bool
testMat2x2ParseLiteral = parse (show (Mat 1 2 3 4)) == Just(Mat 1 2 3 4, "")

testMat2x2ParseAdd :: Bool
testMat2x2ParseAdd = parseRing "[[1,2][3,4]] + [[4,3][2,1]]"
                     == Just(Mat 5 5 5 5)

testMat2x2ParseMul :: Bool
testMat2x2ParseMul = parseRing "[[4,3][2,1]] * [[1,2][3,4]]"
                     == Just(Mat 13 20 5 8)

testMat2x2ParseMulNonComm :: Bool
testMat2x2ParseMulNonComm =
       (parseRing "[[4,3][2,1]] * [[1,2][3,4]]" :: Maybe Mat2x2)
    /= (parseRing "[[1,2][3,4]] * [[4,3][2,1]]" :: Maybe Mat2x2)

testMat2x2AddInv :: Bool
testMat2x2AddInv = ((Mat 1 (-4) (-2) 3) `add` addInv (Mat 1 (-4) (-2) 3))
                   == (Mat 0 0 0 0)

instance Ring Bool where
  addId = False
  addInv = id
  mulId = True

  -- | Same behaviour as XOR, but there's no function defined so I chose
  -- not equal.
  add = (/=)
  mul = (&&)

instance Parsable Bool where
  parse = listToMaybe . reads

testBool = testBoolParseLiteral &&
           testBoolParseAdd &&
           testBoolParseMul &&
           testBoolAddInv

testBoolParseLiteral :: Bool
testBoolParseLiteral = parse "True 234" == Just(True, " 234")

testBoolParseAdd :: Bool
testBoolParseAdd = parseRing "True + True" == Just False

testBoolParseMul :: Bool
testBoolParseMul = parseRing "True * True" == Just True

testBoolAddInv :: Bool
testBoolAddInv = addInv True == True &&
                 addInv False == False

distribute :: RingExpr a -> RingExpr a
distribute (Mul z (Add x y)) = Add (Mul z x) (Mul z y)
distribute (Mul (Add x y) z) = Add (Mul x z) (Mul y z)
