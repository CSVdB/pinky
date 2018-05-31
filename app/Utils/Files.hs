module Utils.Files
    ( readFilePretty
    ) where

import Import

import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BL

import Path.IO (forgivingAbsence)

newtype FileException =
    FileIsMissing (Path Abs File)
    deriving (Show, Eq)

instance Exception FileException where
    displayException (FileIsMissing path) =
        "The file " ++ toFilePath path ++ " does not exist."

readFilePretty :: (MonadIO m, MonadThrow m) => Path Abs File -> m ByteString
readFilePretty path = do
    maybeBL <- liftIO $ forgivingAbsence $ BL.readFile $ toFilePath path
    case maybeBL of
        Nothing -> throwM $ FileIsMissing path
        Just bs -> pure bs
