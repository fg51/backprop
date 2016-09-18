module Lib
    ( someFunc
    ) where

import Text.Printf (printf)
import Control.Monad (forM_)

import Learn (learn)
import Neuralnet (forward)



numOfInput  = 3 :: Int
numOfHidden = 3 :: Int


someFunc :: IO ()
someFunc = do
    putStrLn "backpropagation"

    let elementss = readElementss

    let (outputWeights, hiddenWeightss) = learn
            (initOutputWeights numOfInput)
            (initHiddenWeightss numOfHidden numOfInput)
            elementss

    putStrLn "end learn"

    putStrLn $ "output weights:\n" ++ (show outputWeights)
    putStrLn $ "hidden weights:"
    putMultiList hiddenWeightss

    putStrLn "result"
    forM_ elementss $ \i -> do
        let (out, hiddenInput) = forward outputWeights hiddenWeightss i
        putStr $ (show i ) ++ ": "
        printf "%.4f \n" out


readElementss :: [[Double]]
readElementss =
            [ [1, 1, 1, 1]
            , [1, 1, 0, 1]
            , [1, 0, 1, 1]
            , [1, 0, 0, 0]
            , [0, 1, 1, 1]
            , [0, 1, 0, 0]
            , [0, 0, 1, 0]
            , [0, 0, 0, 0]
            ]

initOutputWeights :: Int -> [Double]
initOutputWeights num = [-0.778802, -0.688467, -0.451277, -0.723136]


initHiddenWeightss :: Int -> Int -> [[Double]]
initHiddenWeightss row col =
    [ [ 0.064486,  0.440718, -0.108188, 0.1234]
    , [ 0.934996, -0.791437,  0.399884, 0.1234]
    , [-0.875362,  0.049715,  0.991000, 0.1234]
    ]

putMultiList :: [[Double]] -> IO ()
putMultiList xss = mapM_ print xss

