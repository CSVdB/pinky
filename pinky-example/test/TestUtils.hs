module TestUtils where

import TestImport

import Pinky

runTrain :: State HyperParams a -> HyperParams -> a
runTrain = flip evalState
