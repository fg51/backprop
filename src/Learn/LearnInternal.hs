module Learn.LearnInternal (learn, aLearnLoopElements, aLearn, learnOutput, learnHidden) where

import Neuralnet (forward)

import Debug.Trace


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
                aLearnLoopElements 0 outputWeights hiddenWeightss elementss
        -- let (err2, outputWeights2, hiddenWeightss2) = do
        --          aLearnLoopElements 0 outputWeights1 hiddenWeightss1 elementss
        -- let (err3, outputWeights3, hiddenWeightss3) = do
        --          aLearnLoopElements 0 outputWeights2 hiddenWeightss2 elementss
        -- (outputWeights3, hiddenWeightss3)
        learnLoop err1 outputWeights1 hiddenWeightss1 elementss
    | otherwise = (outputWeights, hiddenWeightss)
  where
    limit = 0.001


aLearnLoopElements :: Double -> [Double] -> [[Double]] -> [[Double]]
    -> (Double, [Double], [[Double]])
aLearnLoopElements err outputWeights hiddenWeightss [] = do
    -- let msg0 = "in aLearnLoopElements \n"
    -- let msg1 = "err: " ++ (show err) ++ "\n"
    -- let msg2 = "output weight: \n" ++ (show outputWeights) ++ "\n"
    -- let msg3 = "hidden weight: \n" ++ (show hiddenWeightss) ++ "\n"
    -- trace (msg0 ++ msg1 ++ msg2 ++ msg3) (err, outputWeights, hiddenWeightss)
    -- trace (msg1) (err, outputWeights, hiddenWeightss)
    (err, outputWeights, hiddenWeightss)
aLearnLoopElements err outputWeights hiddenWeightss (es:ess) = do
    let (err1, outputWeights1, hiddenWeightss1) = aLearn err outputWeights hiddenWeightss es
    aLearnLoopElements err1 outputWeights1 hiddenWeightss1 ess


aLearn :: Double -> [Double] -> [[Double]] -> [Double]
    -> (Double, [Double], [[Double]])
aLearn err outputWeights hiddenWeightss elements = do
    let (out, hiddenInputs) = forward outputWeights hiddenWeightss $ init elements
    let outputWeights1  = learnOutput outputWeights hiddenInputs elements out
    let hiddenWeightss1 = learnHidden hiddenWeightss outputWeights1 hiddenInputs elements out
    let err1 = err + (out - ans) * (out - ans)
    (err1, outputWeights1, hiddenWeightss1)
      where
        ans = last elements


learnOutput :: [Double] -> [Double] -> [Double] -> Double -> [Double]
learnOutput outputWeights hiddenInputs elements out = do
    let ows = [i + alpha * j * d | (i, j) <- zip outputWeights hiddenInputs]
    let threshold = (last outputWeights) - alpha * d
    let ret = ows ++ [threshold]
    ret
    -- trace ("learnOutput: " ++ (show ret)) ret
      where
        alpha = gALPHA
        ans   = last elements
        diff  = ans - out
        df    = out * (1 - out)
        d     = diff * df


learnHidden :: [[Double]] -> [Double] -> [Double] -> [Double] -> Double
    -> [[Double]]
learnHidden hiddenWeightss outputWeights hiddenInputs elements out = do
    let ret = [learnHidden1 out hi ow hws elements | (hi, ow, hws)
            <- zip3 hiddenInputs outputWeights hiddenWeightss]
    -- trace ("learnHidden: " ++ (show ret)) ret
    ret


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


