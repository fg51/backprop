module Neuralnet (forward) where


forward :: [Double] -> [[Double]] -> [Double] -> (Double, [Double])
forward outputWeights hiddenWeightss elements = do
    let hiddenInputs = calHiddenInputs hiddenWeightss elements
    let out = forward' hiddenInputs outputWeights
    (out, hiddenInputs)


calHiddenInputs :: [[Double]] -> [Double] -> [Double]
calHiddenInputs hiddenWeightss elements = do
    map (forward' elements) hiddenWeightss


forward' :: Num a => [Double] -> [Double] -> Double
forward' inns weights = do
    let total = sum [i * j | (i, j) <- zip inns weights1]
    sigmoid $ total - threshold
  where
    weights1 = init weights
    threshold = last weights


sigmoid :: Double -> Double
sigmoid u = 1.0 / (1.0 + exp (-u))
