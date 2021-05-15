
{-# LANGUAGE ExtendedDefaultRules #-}
module BodySpec (spec) where
import           Test.Hspec

import qualified Luukasa.Body  as B
import           Luukasa.Joint (Joint (Joint))
import qualified Luukasa.Joint as J
import qualified Tree          as T

mkJointAt :: Int -> Double -> Double -> Joint
mkJointAt jointId x y = Joint
    { J.jointId = jointId
    , J.jointX = x
    , J.jointY = y
    , J.jointLocalRot = 0
    , J.jointWorldRot = 0
    , J.jointR = 0
    , J.jointName = Nothing
    }


spec :: Spec
spec = do

    -- First "row" of joints; siblings at Y=10. (Root joint will be created with Body.create.)
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
            B.getParentUnsafe testBody 1 `shouldSatisfy` (\j -> J.jointId j == 0)
            B.getParentUnsafe testBody 2 `shouldSatisfy` (\j -> J.jointId j == 0)
            B.getParentUnsafe testBody 3 `shouldSatisfy` (\j -> J.jointId j == 1)
