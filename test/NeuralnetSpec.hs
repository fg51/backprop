module NeuralnetSpec (spec) where

import Test.Hspec

import Neuralnet (forward)


spec :: Spec
spec = do
    describe "test forward" $
        context "when values was given" $ do
            it "elementss[0] should return out = 0.251957" $ do
                let (out, hi) = forward initOutputWeights initHiddenWeightss $ initElementss !! 0
                abs(out - expectOut0) <= epsillon `shouldBe` True
                [abs(i - j) <= epsillon | (i, j) <- zip hi expectHiddenInputs0]
                    `shouldBe` [True, True, True]

            it "elementss[1] should return out = 0.281609" $ do
                let (out, hi) = forward initOutputWeights initHiddenWeightss $ initElementss !! 1
                abs(out - expectOut1) <= epsillon `shouldBe` True
                [abs(i - j) <= epsillon | (i, j) <- zip hi expectHiddenInputs1] 
                    `shouldBe` [True, True, True]

            it "elementss[2] should return out = 0.247459" $ do
                let (out, hi) = forward initOutputWeights initHiddenWeightss $ initElementss !! 2
                abs(out - expectOut2) <= epsillon `shouldBe` True
                [abs(i - j) <= epsillon | (i, j) <- zip hi expectHiddenInputs2]
                    `shouldBe` [True, True, True]

            it "elementss[3] should return out = 0.273658" $ do
                let (out, hi) = forward initOutputWeights initHiddenWeightss $ initElementss !! 3
                abs(out - expectOut3) <= epsillon `shouldBe` True
                [abs(i - j) <= epsillon | (i, j) <- zip hi expectHiddenInputs3]
                    `shouldBe` [True, True, True]

            it "elementss[4] should return out = 0.273568" $ do
                let (out, hi) = forward initOutputWeights initHiddenWeightss $ initElementss !! 4
                abs(out - expectOut4) <= epsillon `shouldBe` True
                [abs(i - j) <= epsillon | (i, j) <- zip hi expectHiddenInputs4]
                    `shouldBe` [True, True, True]

            it "elementss[5] should return out = 0.296315" $ do
                let (out, hi) = forward initOutputWeights initHiddenWeightss $ initElementss !! 5
                abs(out - expectOut5) <= epsillon `shouldBe` True
                [abs(i - j) <= epsillon | (i, j) <- zip hi expectHiddenInputs5]
                    `shouldBe` [True, True, True]

            it "elementss[6] should return out = 0.258488" $ do
                let (out, hi) = forward initOutputWeights initHiddenWeightss $ initElementss !! 6
                abs(out - expectOut6) <= epsillon `shouldBe` True
                [abs(i - j) <= epsillon | (i, j) <- zip hi expectHiddenInputs6]
                    `shouldBe` [True, True, True]

            it "elementss[7] should return out = 0.289017" $ do
                let (out, hi) = forward initOutputWeights initHiddenWeightss $ initElementss !! 7
                abs(out - expectOut7) <= epsillon `shouldBe` True
                [abs(i - j) <= epsillon | (i, j) <- zip hi expectHiddenInputs7]
                    `shouldBe` [True, True, True]


              where
                epsillon = 0.001

                expectOut0 = 0.251957 :: Double
                expectHiddenInputs0 = [0.567980, 0.603494, 0.510487 :: Double]

                expectOut1 = 0.281609 :: Double
                expectHiddenInputs1 = [0.594308, 0.505040, 0.279077 :: Double]

                expectOut2 = 0.247459 :: Double
                expectHiddenInputs2 = [0.458321, 0.770561, 0.498060 :: Double]

                expectOut3 = 0.273658 :: Double
                expectHiddenInputs3 = [0.485276, 0.692449, 0.269185 :: Double]

                expectOut4 = 0.266996
                expectHiddenInputs4 = [0.552093, 0.374033, 0.714495]

                expectOut5 = 0.296315
                expectHiddenInputs5 = [0.578670, 0.286011, 0.481587]

                expectOut6 = 0.258488
                expectHiddenInputs6 = [0.442360, 0.568684, 0.704246]

                expectOut7 = 0.289017
                expectHiddenInputs7 = [0.469189, 0.469189, 0.469189]


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


