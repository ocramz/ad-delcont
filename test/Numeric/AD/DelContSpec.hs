module Numeric.AD.DelContSpec where

import Numeric.AD.DelCont (grad, rad1)
import Test.Hspec (Spec, hspec, describe, it, shouldBe, shouldSatisfy)


spec :: Spec
spec =
  describe "rad1 : Unary operations" $ do
    let
      x0 = 1.2
    it "(** 2)" $ do
      let
        f x = x ** 2
        (_, dfdx) = rad1 f x0
      dfdx `shouldBe` 2.4
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
