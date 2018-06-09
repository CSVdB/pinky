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

dataDir :: MonadIO m => m (Path Abs Dir)
dataDir = resolveDir' "data/"

notEnoughData :: MonadIO m => String -> m a
notEnoughData action =
    error $ "There are not enough data points for " ++ action ++ "."

load :: MonadIO m => Int -> Int -> Int -> m (MNISTData, MNISTData, MNISTData)
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

getDataSet :: MonadIO m => [Path Abs File] -> String -> m MNISTData
getDataSet files name =
    case getFilesFromList files name of
        Nothing -> error $ "There is no dataset with name " ++ name ++ "."
        Just (trainImageFile, trainLabelFile) -> do
            imagesAsBL <- readFilePretty trainImageFile
            labelsAsBL <- readFilePretty trainLabelFile
            case loadDataSet =<< (,) <$> imagesAsBL <*> labelsAsBL of
                Left errMess -> error errMess
                Right dataset -> pure dataset

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

loadDataSet :: (ByteString, ByteString) -> Either String MNISTData
loadDataSet (imageBL, labelBL) = do
    images <- runParserPretty imageFileParser imageBL
    labels <- runParserPretty labelFileParser labelBL
    NEL.fromList <$> images >< labels
