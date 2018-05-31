module MNIST.Load
    ( load
    ) where

import Import

import MNIST.DataSet
import MNIST.Parser
import Utils.Files
import Utils.Parser
import Utils.Zip

import qualified Data.List.NonEmpty as NEL

data DataSetException
    = NotEnoughDataPoints String
    | MissingDataSet String
    deriving (Show, Eq)

instance Exception DataSetException where
    displayException (NotEnoughDataPoints action) =
        "There are not enough data points for " ++ action ++ "."
    displayException (MissingDataSet name) =
        "There is no data set called " ++ name ++ "."

dataDir :: MonadIO m => m (Path Abs Dir)
dataDir = resolveDir' "data/"

notEnoughData :: MonadThrow m => String -> m a
notEnoughData = throwM . NotEnoughDataPoints

load ::
       (MonadIO m, MonadThrow m)
    => Int
    -> Int
    -> Int
    -> m (MNISTData, MNISTData, MNISTData)
load nOfTrain nOfVal nOfTest = do
    dir <- dataDir
    (_, files) <- liftIO $ listDir dir
    fullTrainDataSet <- getDataSet files "train"
    fullTestDataSet <- getDataSet files "test"
    let (trainSet, valSet) =
            take nOfVal <$> NEL.splitAt nOfTrain fullTrainDataSet
        testSet = NEL.take nOfTest fullTestDataSet
    when (length trainSet /= nOfTrain) $ notEnoughData "training"
    when (length valSet /= nOfVal) $ notEnoughData "validating"
    when (length testSet /= nOfTest) $ notEnoughData "testing"
    pure (NEL.fromList trainSet, NEL.fromList valSet, NEL.fromList testSet)

getDataSet ::
       (MonadIO m, MonadThrow m) => [Path Abs File] -> String -> m MNISTData
getDataSet files name =
    case getFilesFromList files name of
        Nothing -> throwM $ MissingDataSet name
        Just (trainImageFile, trainLabelFile) -> do
            imagesAsBL <- readFilePretty trainImageFile
            labelsAsBL <- readFilePretty trainLabelFile
            loadDataSet (imagesAsBL, labelsAsBL)

getFilesFromList ::
       [Path Abs File] -> String -> Maybe (Path Abs File, Path Abs File)
getFilesFromList paths name = do
    imagePath <-
        find (\path -> toRelFilePath path == name ++ "-images-idx3-ubyte") paths
    labelPath <-
        find (\path -> toRelFilePath path == name ++ "-labels-idx1-ubyte") paths
    pure (imagePath, labelPath)

toRelFilePath :: Path Abs File -> FilePath
toRelFilePath = toFilePath . filename

loadDataSet :: MonadThrow m => (ByteString, ByteString) -> m MNISTData
loadDataSet (imageBL, labelBL) = do
    images <- runParserPretty imageFileParser imageBL
    labels <- runParserPretty labelFileParser labelBL
    NEL.fromList <$> images >< labels
