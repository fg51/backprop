module Learn.LearnInternalSpec (spec) where

import Test.Hspec

import Learn.LearnInternal

-- hiddenWeightss
-- 0.072089 0.448321 -0.100585 0.115797 0.989778 -0.736655 0.454666 0.068618 -0.780786 0.144291 1.085576 0.028824 
-- 
-- outputWeights:
-- 0.021978 0.162382 0.268444 -1.409872 


spec :: Spec
spec = do
    -- describe "test aLearn" $
    --      context "when learnOutput was given" $ do
    --          it "learnOutput should return [0.021978, 0.162382, 0.268444, -1.409872]" $ do
    --              let ans = learnOutput outputWeights hiddenInputs elements out
    --              ans `shouldBe` expectOutput
                 -- [abs(i - j) < epsillon | (i, j) <- zip ans expectOutput] `shouldBe` [True, True, True, True]

    --         it "learnHidden should return " $ do
    --             let ans = learnHidden hiddenWeightss expectOutput hiddenInputs elements out
    --             [[abs(i - j) < epsillon | (i, j) <- zip ii jj] | (ii, jj) <- zip ans expectHidden] `shouldBe` [[True, True, True, True], [True, True, True, True], [True, True, True, True]]

    describe "test aLearn" $
        context "when elementss was given" $ do
            it "elementss[0] should return err 0.559568" $ do
                let (err, ows, hwss) = aLearn 0
                        initOutputWeights initHiddenWeightss $ initElementss !! 0
                abs(err - err1) < epsillon `shouldBe` True
                diffList epsillon ows getOutputWeights1 `shouldBe` True
                diffMultiList epsillon hwss getHiddenWeightss1 `shouldBe` True

            it "elementss[1] should return err 0.587652" $ do
                let (err, ows, hwss) = aLearn err1
                        getOutputWeights1 getHiddenWeightss1 $ initElementss !! 1
                abs(err - err2) < epsillon `shouldBe` True
                diffList epsillon ows getOutputWeights2 `shouldBe` True
                diffMultiList epsillon hwss getHiddenWeightss2 `shouldBe` True

            it "elementss[2] should return err 0.598467" $ do
                let (err, ows, hwss) = aLearn err2
                        getOutputWeights2 getHiddenWeightss2 $ initElementss !! 2
                abs(err - err3) < epsillon `shouldBe` True
                diffList epsillon ows getOutputWeights3 `shouldBe` True
                diffMultiList epsillon hwss getHiddenWeightss3 `shouldBe` True

            it "elementss[3] should return err 1.415170" $ do
                let (err, ows, hwss) = aLearn err3
                        getOutputWeights3 getHiddenWeightss3 $ initElementss !! 3
                abs(err - err4) < epsillon `shouldBe` True
                diffList epsillon ows getOutputWeights4 `shouldBe` True
                diffMultiList epsillon hwss getHiddenWeightss4 `shouldBe` True

            it "elementss[4] should return err 1.501098" $ do
                let (err, ows, hwss) = aLearn err4
                        getOutputWeights4 getHiddenWeightss4 $ initElementss !! 4
                abs(err - err5) < epsillon `shouldBe` True
                diffList epsillon ows getOutputWeights5 `shouldBe` True
                diffMultiList epsillon hwss getHiddenWeightss5 `shouldBe` True

            it "elementss[5] should return err 2.283289" $ do
                let (err, ows, hwss) = aLearn err5
                        getOutputWeights5 getHiddenWeightss5 $ initElementss !! 5
                abs(err - err6) < epsillon `shouldBe` True
                diffList epsillon ows getOutputWeights6 `shouldBe` True
                diffMultiList epsillon hwss getHiddenWeightss6 `shouldBe` True

            it "elementss[6] should return err 2.643080" $ do
                let (err, ows, hwss) = aLearn err6
                        getOutputWeights6 getHiddenWeightss6 $ initElementss !! 6
                abs(err - err7) < epsillon `shouldBe` True
                diffList epsillon ows getOutputWeights7 `shouldBe` True
                diffMultiList epsillon hwss getHiddenWeightss7 `shouldBe` True

            it "elementss[7] should return err 2.647083" $ do
                let (err, ows, hwss) = aLearn err7
                        getOutputWeights7 getHiddenWeightss7 $ initElementss !! 7
                abs(err - err8) < epsillon `shouldBe` True
                diffList epsillon ows getOutputWeights8 `shouldBe` True
                diffMultiList epsillon hwss getHiddenWeightss8 `shouldBe` True


    describe "test aLearnLoopElements" $
        it "aLearnLoopElements 1" $ do
            let (err, ows, hwss) = aLearnLoopElements 0
                    initOutputWeights initHiddenWeightss initElementss
            abs(err - err8) < 0.01 `shouldBe` True
            diffList epsillon ows getOutputWeights8 `shouldBe` True
            diffMultiList epsillon hwss getHiddenWeightss8 `shouldBe` True

        it "aLearnLoopElements 2" $ do
            let (err, ows, hwss) = aLearnLoopElements 0
                    initOutputWeights initHiddenWeightss initElementss
            abs(err - err8) < 0.01 `shouldBe` True
            diffList epsillon ows getOutputWeights8 `shouldBe` True
            diffMultiList epsillon hwss getHiddenWeightss8 `shouldBe` True


  where
    expectOutput = getExpectOutput
    expectHidden = getExpectHidden

    outputWeights = initOutputWeights
    hiddenInputs = [0.567980, 0.603494, 0.510487]
    hiddenWeightss = initHiddenWeightss
    --hiddenInputs = [0.064486, 0.440718, -0.108188, 0.123400, 0.934996, -0.791437, 0.399884, 0.123400, -0.875362, 0.049715, 0.991000, 0.123400]
    elements = [1, 1, 1, 1]
    out = 0.251957



diffMultiList :: Double -> [[Double]] -> [[Double]] -> Bool
diffMultiList epsillon xs ys = and [all (< epsillon) i | i <- diffss]
  where
    diffss = [[abs(i - j) | (i, j) <- zip ii jj] | (ii, jj) <- zip xs ys]

diffList :: Double -> [Double] -> [Double] -> Bool
diffList epsillon xs ys = all (< epsillon) [abs(i - j) | (i, j) <- zip xs ys]



epsillon = 0.01 :: Double


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

initOutputWeights :: [Double]
initOutputWeights = [-0.778802, -0.688467, -0.451277, 0.000000]

initHiddenWeightss :: [[Double]]
initHiddenWeightss = 
    [ [ 0.064486,  0.440718, -0.108188, 0.1234]
    , [ 0.934996, -0.791437,  0.399884, 0.1234]
    , [-0.875362,  0.049715,  0.991000, 0.1234]
    ]

getOutputWeights1 :: [Double]
getOutputWeights1 = [0.021978, 0.162382, 0.268444, -1.409872]


getHiddenWeightss1 :: [[Double]]
getHiddenWeightss1 =
    [ [ 0.072089,  0.448321, -0.100585, 0.115797]
    , [ 0.989778, -0.736655,  0.454666, 0.068618]
    , [-0.780786,  0.144291,  1.085576, 0.028824]
    ]
err1 = 0.559568 :: Double


getOutputWeights2 :: [Double]
getOutputWeights2 = [0.162194, 0.290021, 0.347821, -1.643646]

getHiddenWeightss2 :: [[Double]]
getHiddenWeightss2 = [
      [ 0.081191,  0.457423, -0.100585, 0.106695]
    , [ 1.006585, -0.719848,  0.454666, 0.051811]
    , [-0.762551,  0.162526,  1.085576, 0.010589]
    ]
err2 = 0.587652 :: Double


getOutputWeights3 = [0.207597, 0.367903, 0.403782, -1.740552]
getHiddenWeightss3 = [
      [ 0.086200,  0.457423, -0.095575, 0.101686]
    , [ 1.012210, -0.719848,  0.460291, 0.046186]
    , [-0.753004,  0.162526,  1.095124, 0.001042]
    ]
err3 = 0.598467 :: Double


getOutputWeights4 = [-0.182536, -0.201673, 0.152196, -0.954199]
getHiddenWeightss4 = [
      [0.122082, 0.457423, -0.095575, 0.065804]
    , [1.043876, -0.719848, 0.460291, 0.014520]
    , [-0.779044, 0.162526, 1.095124, 0.027082]
    ]
err4 = 1.415170 :: Double


getOutputWeights5 = [0.165792, 0.060666, 0.622273, -1.561597]
getHiddenWeightss5 = [
      [0.122082, 0.482055, -0.070943, 0.041172]
    , [1.043876, -0.710807, 0.469333, 0.005479]
    , [-0.779044, 0.228658, 1.161256, -0.039051]
    ]
err5 = 1.501098


getOutputWeights6 = [-0.384320, -0.236067, 0.110077, -0.657506]
getHiddenWeightss6 = [
      [0.122082, 0.564832, -0.070943, -0.041605]
    , [1.043876, -0.663749, 0.469333, -0.041579]
    , [-0.779044, 0.204219, 1.161256, -0.014611]
    ]
err6 = 2.283289


getOutputWeights7 = [-1.093656, -1.135966, -0.990216, 0.782285]
getHiddenWeightss7 = [
      [0.122082, 0.564832, 0.322631, -0.435179]
    , [1.043876, -0.663749, 0.852657, -0.424904]
    , [-0.779044, 0.204219, 1.418163, -0.271518]
    ]
err7 = 2.643080


getOutputWeights8 = [-1.116419, -1.158637, -1.011493, 0.819779]
getHiddenWeightss8 = [
      [0.122082, 0.564832, 0.322631, -0.445164]
    , [1.043876, -0.663749, 0.852657, -0.435289]
    , [-0.779044, 0.204219, 1.418163, -0.280827]
    ]
err8 = 2.647083

