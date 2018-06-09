module Utils.Files
    ( readFilePretty
    ) where

import Import

import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BL

import Path.IO (forgivingAbsence)

readFilePretty :: MonadIO m => Path Abs File -> m (Either String ByteString)
readFilePretty path = do
    maybeBL <- liftIO $ forgivingAbsence $ BL.readFile $ toFilePath path
    case maybeBL of
        Nothing ->
            pure . Left $ "The file " ++ toFilePath path ++ " does not exist."
        Just bs -> pure $ Right bs
