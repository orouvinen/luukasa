
{-# LANGUAGE ExtendedDefaultRules #-}
module BodySpec (spec) where
import           Data.Maybe
import           Test.Hspec

import qualified Body       as B
import           Joint
import qualified Tree       as T

mkJointAt :: Int -> Double -> Double -> Joint
mkJointAt jointId x y = Joint { jointId = jointId, jointX = x, jointY = y, jointLocalRot = 0, jointWorldRot = 0, jointR = 0 }


spec :: Spec
spec = do
    -- Root joint
    let j0 = mkJointAt 0 0 0
    -- First "row" of joints; siblings at Y=10
    let j1 = mkJointAt 1 10 10
    let j2 = mkJointAt 2 20 10
    -- Second row, child of jointId 1
    let j3 = mkJointAt 3 30 20

    let body = B.create
        rootJoint = T.val $ B.root body
        b1 = B.addJoint body rootJoint j1
        b2 = B.addJoint b1 rootJoint j2
        b3 = B.addJoint b2 j1 j3
        testBody = b3

    describe "Body" $ do
        it "has correct limb segments" $ do
            let res = B.limbSegments testBody
            length res `shouldBe` 3

            ((0, 0), (10, 10)) `elem` res `shouldBe` True
            ((0, 0), (20, 10)) `elem` res `shouldBe` True
            ((10, 10), (30, 20)) `elem` res `shouldBe` True

        it "returns correct joint coordinates" $ do
            let res = B.jointPositions testBody

            (0, 0) `elem` res `shouldBe` True
            (10, 10) `elem` res `shouldBe` True
            (20, 10) `elem` res `shouldBe` True
            (30, 20) `elem` res `shouldBe` True

        it "adding joints updates parent lookup" $ do
            B.getParent testBody 1 `shouldSatisfy` (\j -> jointId j == 0)
            B.getParent testBody 2 `shouldSatisfy` (\j -> jointId j == 0)
            B.getParent testBody 3 `shouldSatisfy` (\j -> jointId j == 1)
