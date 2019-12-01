module Spec (spec) where

import Control.Egison
import Test.Hspec


spec :: Spec
spec = do
  describe "list and multiset matchers" $ do
    it "cons pattern for list" $
      matchAll [1,2,3] (List Integer) [[mc| cons $x $xs => (x, xs) |]]
      `shouldBe` [(1, [2,3])]
    it "tuple = pair" $
      matchAll ([1,2,3], [4,5]) (Pair (List Integer) (List Integer))
        [[mc| (pair (cons $x $xs) (cons $y _)) => (x, y) |]]
      `shouldBe` [(1, 4)]

  describe "built-in pattern constructs" $ do
    it "Predicate patterns" $
      matchAll [1..10] (Multiset Integer)
        [[mc| cons (& (PredicatePat (\x -> mod x 2 == 0)) $x) _ => x |]]
      `shouldBe` [2,4,6,8,10]
    it "★mini7" $
      let { w = 3; min = 0; max = 7 }
      in match w Integer
        [[mc| PredicatePat (< min) => (w, max)   |],
         [mc| PredicatePat (> max) => (min, w)   |],
         [mc| _                    => (min, max) |]]
      `shouldBe` (0, 7)



