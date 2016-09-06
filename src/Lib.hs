{-# LANGUAGE TemplateHaskell #-}
module Lib
    ( someFunc
    ) where


import Development.Placeholders


numOfInput  = 3 :: Int
numOfHidden = 3 :: Int


someFunc :: IO ()
someFunc = do
    putStrLn "backpropagation"

    let elementss =
            [ [1, 1, 1, 1]
            , [1, 1, 0, 1]
            , [1, 0, 1, 1]
            , [1, 0, 0, 0]
            , [0, 1, 1, 1]
            , [0, 1, 0, 0]
            , [0, 0, 1, 0]
            , [0, 0, 0, 0 :: Double]
            ]

    let (outputWeights, hiddenWeightss) = learn
            (initOutputWeights numOfInput)
            (initHiddenWeightss numOfHidden numOfInput)
            elementss

    putStrLn "end learn"

    forwardLoop outputWeights hiddenWeightss elementss



-- size = num + 1
initOutputWeights :: Int -> [Double]
initOutputWeights num = [-0.778802, -0.688467, -0.451277, -0.723136]


initHiddenWeightss :: Int -> Int -> [[Double]]
-- initHiddenWeightss row col = $notImplemented
initHiddenWeightss row col =
    [ [ 0.064486,  0.440718, -0.108188]
    , [ 0.934996, -0.791437,  0.399884]
    , [-0.875362,  0.049715,  0.991000]
    ]


learn :: Num a => [a] -> [[a]] -> [[a]] -> ([a], [[a]])
learn = $notImplemented


forwardLoop :: Num a => [a] -> [[a]] -> [[a]] -> IO ()
forwardLoop outputWeights hiddenWeightss elementss = $notImplemented
--forwardLoop outputWeights hiddenWeightss elementss = do
--     out = forward outputWeights hiddenWeightss elements

forward :: Num a => [a] -> [[a]] -> [a] -> a
forward outputWeights hiddenWeightss elements = $notImplemented

