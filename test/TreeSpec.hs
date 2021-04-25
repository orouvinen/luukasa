{-# LANGUAGE ExtendedDefaultRules #-}
module TreeSpec (spec) where
import           Data.Foldable (toList)
import           Data.Maybe
import           Test.Hspec

import qualified Tree          as T


spec :: Spec
spec = do
    describe "Tree" $ do
{-
Test tree:

              Root
            /      \
            L1c1    L1c2
            |         |
            L2c1    Empty
          /    \
        L3c1   L3c2
-}

        let root = T.create "rootNode"
        -- Siblings at first level
        let l1_child_1 = T.insert "L1 child1" (== "rootNode") root
        let l1_child_2 = T.insert "L1 child2" (== "rootNode") l1_child_1

        -- Level 2 (delete test)
        let l2_child_1 = T.insert "L2 child1" (== "L1 child1") l1_child_2

        -- Level 3
        let l3_child_1 = T.insert "L3 child1" (== "L2 child1") l2_child_1
        let l3_child_2 = T.insert "L3 child2" (== "L2 child1") l3_child_1
        let finalTree = l3_child_2

        let deleteTestTree = l3_child_2
        let replaceTestTree = T.replaceValBy (== "L1 child1") "REPLACED" l3_child_2

        let noInsert = T.insert "nope" (== "invalidParent") l1_child_2

        describe "create" $ do
            it "creates a node" $ do
                T.val root `shouldBe` "rootNode"
                T.children root `shouldSatisfy` null

        describe "insert" $ do
            it "inserts 1st child" $ do
                length (T.children l1_child_1) `shouldBe` 1
                head (T.children l1_child_1) `shouldSatisfy` (\x -> T.val x == "L1 child1")

            it "inserts 2nd child" $ do
                length (T.children l1_child_2) `shouldBe` 2
                length (filter (\x -> T.val x == "L1 child2") (T.children l1_child_2))
                    `shouldBe` 1

            it "doesn't insert with non-existing parent" $ do
                length noInsert `shouldBe` 3

        describe "delete" $ do
            let res = T.delete (\x -> T.val x == "L2 child1") deleteTestTree
            it "deletes a node" $ do
                T.findNodeBy (== "L2 child1") res `shouldBe` Nothing

            it "deletes only one node" $ do
                length res `shouldBe` length deleteTestTree - 1

            it "extends deleted node's parent's children with deleted node's children" $ do
                let l1c1 = T.findNodeBy (== "L1 child1") res
                l1c1 `shouldSatisfy` isJust
                length (T.children (fromJust l1c1)) `shouldBe` 2

        describe "replaceValBy" $ do
            it "replaces single node's value" $ do
                let replacedNode = T.findNodeBy (== "REPLACED") replaceTestTree
                -- make sure there is the replaced node
                replacedNode `shouldSatisfy` isJust
                -- make sure size of tree didn't get affected
                length replaceTestTree `shouldBe` length finalTree
                -- finally, there should be exactly one node replaced in this particular case
                let nodes = toList replaceTestTree
                length [ n | n <- nodes, n == "REPLACED"] `shouldBe` 1

