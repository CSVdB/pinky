module Utils.Parser
    ( runParserPretty
    , runParserPrettyWithFilePath
    , word32Parser
    , Parser
    ) where

import Import

import Text.Megaparsec (Parsec)
import qualified Text.Megaparsec as MP
import qualified Text.Megaparsec.Byte as MP

type Parser a = Parsec Void ByteString a

runParserPretty :: MonadThrow m => Parser a -> ByteString -> m a
runParserPretty = runParserPrettyWithFilePath ""

runParserPrettyWithFilePath ::
       MonadThrow m => FilePath -> Parser a -> ByteString -> m a
runParserPrettyWithFilePath filepath parser t =
    case MP.runParser parser filepath t of
        Left err -> throwM err
        Right x -> pure x

word32Parser :: Parser Word32
word32Parser = do
    a <- fromIntegral <$> MP.anyChar
    b <- fromIntegral <$> MP.anyChar
    c <- fromIntegral <$> MP.anyChar
    d <- fromIntegral <$> MP.anyChar
    pure $ shiftL a 24 + shiftL b 16 + shiftL c 8 + d
