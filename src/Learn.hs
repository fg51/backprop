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
learnHidden hiddenWeightss outputWeights hiddenInputs elements out = do
    [learnHidden1 out hi ow hws elements | (hi, ow, hws)
        <- zip3 hiddenInputs outputWeights hiddenWeightss]


learnHidden1 :: Double -> Double -> Double -> [Double] -> [Double] -> [Double]
learnHidden1 out hiddenInput outputWeight hiddenWeights elements = weights ++ [threshold]
  where
    ans = last elements
    diff = calHiddenDiff hiddenInput outputWeight ans out

    weights = [learnHiddenWeight diff i j | (i, j) <-
        zip (init hiddenWeights) (init elements)]
    threshold = learnHiddenThreshold diff $ last hiddenWeights


calHiddenDiff :: Double -> Double -> Double -> Double -> Double
calHiddenDiff hiddenInput outputWeight ans out = do
    hiddenInput * (1 - hiddenInput) * outputWeight * diff * fd
  where
    fd = out * (1 - out)
    diff = ans - out


learnHiddenWeight :: Double -> Double -> Double -> Double
learnHiddenWeight diff weight element = do
    weight + alpha * element * diff
  where
    alpha = gALPHA


learnHiddenThreshold :: Double -> Double -> Double
learnHiddenThreshold diff threshold = do
    threshold - alpha * diff
  where
    alpha = gALPHA

