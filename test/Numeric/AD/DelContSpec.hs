{-# options_ghc -Wno-type-defaults -Wno-unused-imports -Wno-missing-methods -Wno-orphans #-}
module Numeric.AD.DelContSpec where

import Numeric.AD.DelCont (grad, rad1)
import qualified Numeric.AD as AD (grad)
import Test.Hspec (Spec, hspec, describe, it, shouldBe, shouldSatisfy)


spec :: Spec
spec = do
  describe "rad1 : Unary functions" $ do
    let
      x0 = 1.2
      x1 = 0.0
    it "(** 2)" $ do
      let
        f x = x ** 2
        (_, dfdx) = rad1 f x0
        (_, dfdx1) = rad1 f x1
      dfdx `shouldBe` 2.4
      dfdx1 `shouldBe` 0.0
    it "sqrt" $ do
      let
        (_, dfdx) = rad1 sqrt x0
        xhat = 1 / (2 * (sqrt x0))
      dfdx `shouldBe` xhat
    it "reciprocal" $ do
      let
        (_, dfdx) = rad1 recip x0
        xhat = negate $ 1 / (x0 **2)
      dfdx `shouldBe` xhat
  describe "grad : Multivariate functions" $ do
    let
      x = [1.2, 1.3]
    it "inverse of the L2 norm" $ do
      let
        f z = 1 / norm z
        (_, gradf) = grad f x -- 'ad-delcont'
        gradAD = AD.grad f x -- 'ad'
      norm (gradf - gradAD) `shouldSatisfy` (<= 1e-12)

instance Num a => Num [a] where
  (-) = zipWith (-)

norm :: Floating a => [a] -> a
norm = sqrt . dot

dot :: Num c => [c] -> c
dot xs = sum $ zipWith (*) xs xs
