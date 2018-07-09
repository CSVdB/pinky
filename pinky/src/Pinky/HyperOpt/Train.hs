{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ConstraintKinds #-}

module Pinky.HyperOpt.Train where

import Import

import Pinky.Core
import Pinky.Core.HyperParams.Internal
import Pinky.ParamOpt
import Pinky.ParamOpt.Train
import Pinky.Utils
import Pinky.Utils.PositiveDouble.Internal
import Pinky.Utils.PositiveInt.Internal

import Control.Monad.Random.Lazy
import Control.Monad.State.Lazy
import Control.Monad.Trans.Reader

import Data.Function (on)

import Data.Singletons.Prelude (Head, Last)

type HyperOptCondition net layers shapes i o m
     = ( i ~ Head shapes
       , o ~ Last shapes
       , SingI o
       , CreateRandom net
       , net ~ Network layers shapes
       , MonadRandom m)

runHyperParams ::
       forall net i o m (layers :: [*]) shapes.
       HyperOptCondition net layers shapes i o m
    => HyperParams
    -> SearchInfo i o
    -> m ClassificationAccuracy
runHyperParams hp (SearchInfo errorFunc epochs trainData valData) = do
    net <- createRandomM @m @(Momentum (Network layers shapes))
    let Momentum trainedNet _ =
            flip runReader errorFunc $
            flip evalStateT hp $ trainNetwork net trainData epochs
    pure $ accuracy trainedNet valData

data UpdateHyperParams = UpdateHyperParams
    { paramExp :: Double -- ^ exponent for possible change in parameters
    , decayRateExp :: Double -- ^ exponent for possible change in decay rate
    , batchSizeFactor :: Natural -- ^ factor for possible change in batchSize
    } deriving (Show, Eq, Generic)

instance Validity UpdateHyperParams where
    validate (UpdateHyperParams x y n) =
        mconcat
            [ declare "paramExp > 0" $ x > 0
            , declare "decayRateExp > 0" $ y > 0
            , declare "batchSizeFactor > 0" $ n > 0
            ]

updateHyperParams ::
       PositiveDouble -> PositiveDouble -> PositiveInt -> UpdateHyperParams
updateHyperParams (PositiveDouble x) (PositiveDouble y) (PositiveInt z) =
    UpdateHyperParams x y z

eitherGenHP ::
       MonadRandom m
    => HyperParams
    -> UpdateHyperParams
    -> m (Either String HyperParams)
eitherGenHP hp@HyperParams {..} (UpdateHyperParams x y z) = do
    let rateLog = log $ posToDouble hyperRate
    rate <- exp <$> getRandomR (-x + rateLog, x + rateLog)
    let decayRateLog = log $ fracToDouble hyperDecayRate
    decayRate <- exp <$> getRandomR (decayRateLog - y, min (decayRateLog + y) 0)
    let momentumLog = log $ fracToDouble hyperMomentum
    momentum <- exp <$> getRandomR (momentumLog - x, min (momentumLog + x) 0)
    let regLog = log $ posToDouble hyperRegulator
    regulator <- exp <$> getRandomR (regLog - x, regLog + x)
    let bsN = posToNum hyperBatchSize
    let minBatchSize =
            if z >= bsN
                then 1
                else bsN - z
    batchSize <-
        naturalFromInteger <$>
        getRandomR (fromIntegral minBatchSize, fromIntegral $ bsN + z)
    pure $ hyperParamsFromBasics rate decayRate momentum regulator batchSize

genHP :: MonadRandom m => HyperParams -> UpdateHyperParams -> m HyperParams
genHP hp uhp = do
    result <- eitherGenHP hp uhp
    case result of
        Left _ -> genHP hp uhp
        Right r -> pure r

localSearchHyperOptIter ::
       forall net i o m layers shapes. HyperOptCondition net layers shapes i o m
    => HyperParams
    -> SearchInfo i o
    -> UpdateHyperParams
    -> m (HyperParams, ClassificationAccuracy)
localSearchHyperOptIter hp si uhp = do
    hp' <- genHP hp uhp
    (hp', ) <$> runHyperParams @net hp' si

data SearchInfo (i :: Shape) (o :: Shape) = SearchInfo
    { sErrorFunc :: ErrorFunc o
    , sEpochs :: Natural
    , sTrainSet :: DataSet i o
    , sValSet :: DataSet i o
    } deriving (Generic, Show)

instance Validity (SearchInfo i o)

data LocalSearchArgs (i :: Shape) (o :: Shape) = LocalSearchArgs
    { lsInfo :: SearchInfo i o
    , lsMinAcc :: ClassificationAccuracy -- ^ desired accuracy
    , lsaUhp :: UpdateHyperParams
    , lsaUhpDecay :: UpdateHyperParams
    } deriving (Generic, Show)

instance Validity (LocalSearchArgs i o)

data RandomSearchArgs (i :: Shape) (o :: Shape) = RandomSearchArgs
    { rsInfo :: SearchInfo i o
    , rsaUhp :: UpdateHyperParams
    } deriving (Generic, Show)

instance Validity (RandomSearchArgs i o)

localSearchHyperOpt ::
       forall net i o layers shapes m.
       ( i ~ Head shapes
       , o ~ Last shapes
       , SingI o
       , CreateRandom net
       , net ~ Network layers shapes
       , MonadRandom m
       )
    => (HyperParams, ClassificationAccuracy)
    -> Natural -- ^ maximal number of hyperparameter sets tested
    -> LocalSearchArgs i o
    -> m (HyperParams, ClassificationAccuracy)
localSearchHyperOpt init maxIter lsa@(LocalSearchArgs si minAcc uhp uhpDecay) =
    ifThenElse (snd init > minAcc) (pure init) $
    case minusNaturalMaybe maxIter 1 of
        Nothing -> pure init
        Just iterLeft -> do
            result <- localSearchHyperOptIter @net (fst init) si uhp
            if result == init
                then localSearchHyperOpt @net init iterLeft lsa
                else localSearchHyperOpt @net init iterLeft newLsa
  where
    uhpNew (UpdateHyperParams x y z) (UpdateHyperParams xDecay yDecay zDecay) =
        UpdateHyperParams (x / xDecay) (y / yDecay) $ z `div` zDecay
    newLsa = lsa {lsaUhp = uhpNew uhp uhpDecay}

ifThenElse :: Bool -> a -> a -> a
ifThenElse True a _ = a
ifThenElse False _ a = a

randomSearchHyperOpt ::
       forall net i o layers shapes m. HyperOptCondition net layers shapes i o m
    => HyperParams
    -> Natural -- ^ number of hyperparameter sets tested
    -> RandomSearchArgs i o
    -> m (HyperParams, ClassificationAccuracy)
randomSearchHyperOpt initHp iter rsa@(RandomSearchArgs si uhp) = do
    hps <- replicateM (fromIntegral iter) $ genHP initHp uhp
    couples <- traverse toTuple hps
    pure $ maximumBy (compare `on` snd) couples
  where
    toTuple :: HyperParams -> m (HyperParams, ClassificationAccuracy)
    toTuple hp = (hp, ) <$> runHyperParams @net hp si
