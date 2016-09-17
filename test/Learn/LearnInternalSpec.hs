module Learn.LearnInternalSpec (main, spec) where

import Test.Hspec

import Learn.LearnInternal

-- hiddenWeightss
-- 0.072089 0.448321 -0.100585 0.115797 0.989778 -0.736655 0.454666 0.068618 -0.780786 0.144291 1.085576 0.028824 
-- 
-- outputWeights:
-- 0.021978 0.162382 0.268444 -1.409872 

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    -- describe "test aLearn" $
    --     context "when learnOutput was given" $ do
    --         it "learnOutput should return [0.021978, 0.162382, 0.268444, -1.409872]" $ do
    --             let ans = learnOutput outputWeights hiddenInputs elements out
    --             [abs(i - j) < epsillon | (i, j) <- zip ans expectOutput] `shouldBe` [True, True, True, True]

    --         it "learnHidden should return " $ do
    --             let ans = learnHidden hiddenWeightss expectOutput hiddenInputs elements out
    --             [[abs(i - j) < epsillon | (i, j) <- zip ii jj] | (ii, jj) <- zip ans expectHidden] `shouldBe` [[True, True, True, True], [True, True, True, True], [True, True, True, True]]

    --         it "aLearn should return err " $ do
    --             let (err1, outputWeights1, hiddenWeightss1) = aLearn 0 outputWeights hiddenWeightss elements
    --             abs(err1 - 0.559568) < 0.01 `shouldBe` True

    --         it "aLearn should return outputWeights" $ do
    --             let (err1, outputWeights1, hiddenWeightss1) = aLearn 0 outputWeights hiddenWeightss elements
    --             outputWeights1 `shouldBe` expectOutput

    --         it "aLearn should return hiddenWeightss" $ do
    --             let (err1, outputWeights1, hiddenWeightss1) = aLearn 0 outputWeights hiddenWeightss elements
    --             hiddenWeightss1 `shouldBe` expectHidden

    describe "test learnLoopElements" $
        context "when learnLoopElements was given" $ do

            -- it "aLearnLoopElements should return " $ do
            --     let (err1, outputWeights1, hiddenWeightss1) = aLearnLoopElements 0 
            --             outputWeights hiddenWeightss initElementss
            --     err1 `shouldBe` 2.647083
            --     -- outputWeights1 `shouldBe` expectOutput

            it "aLearnLoopElements should return " $ do
                let (err1, outputWeights1, hiddenWeightss1) = aLearnLoopElements 0 
                        outputWeights hiddenWeightss initElementss
                outputWeights1 `shouldBe` [-1.116419, -1.158637, -1.011493, 0.819779]

            -- it "aLearnLoopElements should return " $ do
            --     let (err1, outputWeights1, hiddenWeightss1) = aLearnLoopElements 0 
            --             outputWeights hiddenWeightss initElementss
            --     hiddenWeightss1 `shouldBe` [
            --           [ 0.122082,  0.564832, 0.322631, -0.445164]
            --         , [ 1.043876, -0.663749, 0.852657, -0.435289]
            --         , [-0.779044,  0.204219, 1.418163, -0.280827]
            --         ]
-- 


  where
    expectOutput = getExpectOutput
    expectHidden = getExpectHidden

    outputWeights = initOutputWeights
    hiddenInputs = [0.567980, 0.603494, 0.510487]
    hiddenWeightss = initHiddenWeightss
    --hiddenInputs = [0.064486, 0.440718, -0.108188, 0.123400, 0.934996, -0.791437, 0.399884, 0.123400, -0.875362, 0.049715, 0.991000, 0.123400]
    elements = [1, 1, 1, 1]
    out = 0.251957

epsillon = 0.01 :: Double

initOutputWeights :: [Double]
initOutputWeights = [-0.778802, -0.688467, -0.451277, 0.000000]

initHiddenWeightss :: [[Double]]
initHiddenWeightss = 
    [ [ 0.064486,  0.440718, -0.108188, 0.1234]
    , [ 0.934996, -0.791437,  0.399884, 0.1234]
    , [-0.875362,  0.049715,  0.991000, 0.1234]
    ]

getExpectOutput :: [Double]
getExpectOutput = [0.021978, 0.162382, 0.268444, -1.409872]

getExpectHidden :: [[Double]]
getExpectHidden =
    [ [ 0.072089,  0.448321, -0.100585, 0.115797]
    , [ 0.989778, -0.736655,  0.454666, 0.068618]
    , [-0.780786,  0.144291,  1.085576, 0.028824]
    ]


initElementss :: [[Double]]
initElementss =
            [ [1, 1, 1, 1]
            , [1, 1, 0, 1]
            , [1, 0, 1, 1]
            , [1, 0, 0, 0]
            , [0, 1, 1, 1]
            , [0, 1, 0, 0]
            , [0, 0, 1, 0]
            , [0, 0, 0, 0]
            ]

