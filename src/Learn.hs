module Learn where

import Neuralnet (forward)


gALPHA = 10


learn :: [Double] -> [[Double]] -> [[Double]] -> ([Double], [[Double]])
learn outputWeights hiddenWeightss elementss = do
    learnLoop bigErr outputWeights hiddenWeightss elementss
  where
    bigErr = 100


learnLoop :: Double -> [Double] -> [[Double]] -> [[Double]] 
     -> ([Double], [[Double]])
learnLoop err outputWeights hiddenWeightss elementss
    | err >= limit = do
        let (err1, outputWeights1, hiddenWeightss1) = do
                aLearnLoop 0 outputWeights hiddenWeightss elementss
        learnLoop err1 outputWeights1 hiddenWeightss1 elementss
    | otherwise = (outputWeights, hiddenWeightss)
      where
        limit = 0.01


aLearnLoop :: Double -> [Double] -> [[Double]] -> [[Double]]
    -> (Double, [Double], [[Double]])
aLearnLoop err outputWeights hiddenWeightss [] = (err, outputWeights, hiddenWeightss)
aLearnLoop err outputWeights hiddenWeightss (e:es) = do
        let (err1, outputWeights1, hiddenWeightss1) = aLearn err outputWeights hiddenWeightss e
        aLearnLoop err1 outputWeights hiddenWeightss es


aLearn :: Double -> [Double] -> [[Double]] -> [Double]
    -> (Double, [Double], [[Double]])
aLearn err outputWeights hiddenWeightss elements = do
    let (out, hiddenInputs) = forward outputWeights hiddenWeightss elements
    let outputWeights1  = learnOutput outputWeights hiddenInputs elements out
    let hiddenWeightss1 = learnHidden hiddenWeightss outputWeights1 elements

    let err1 = err + (out - ans) * (out - ans)
    (err1, outputWeights, hiddenWeightss)
      where
        ans = last elements


learnOutput :: [Double] -> [Double] -> [Double] -> Double -> [Double]
learnOutput outputWeights hiddenInputs elements out = do
    let ows = [i + alpha * j * d | (i, j) <- zip outputWeights hiddenInputs]
    let threshold = (last outputWeights) - alpha * d
    ows ++ [threshold]
      where
        alpha = gALPHA
        ans = last elements
        diff = ans - out
        df = out * (1 - out)
        d = diff * df


learnHidden :: [[Double]] -> [Double] -> [Double] -> [Double] -> Double -> [[Double]]
learnHidden hiddenWeightss outputWeights hiddenInputs elements out = undefined

--learnHidden2 :: [[Double]] -> [[Double]] -> Double -> [Double]
--learnHidden2 hiddenWeightss elementss out = do
--    let diff = hi * (1 - hi) * ow * (ans - out) * out * (1 - out)
--    hiddenWeights1 = learnHidden1 hiddenWeights elements diff
--
--learnHidden1 :: [Double] -> [Double] -> Double -> [Double]
--learnHidden1 hiddenWeights elements diff = do
--    [i + alpha * j * diff | (i, j) <- zip hiddenWeights elements]
--  where
--    alpha = gALPHA


