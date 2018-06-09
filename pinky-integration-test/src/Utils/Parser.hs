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

runParserPretty :: Parser a -> ByteString -> Either String a
runParserPretty = runParserPrettyWithFilePath ""

runParserPrettyWithFilePath ::
       FilePath -> Parser a -> ByteString -> Either String a
runParserPrettyWithFilePath filepath parser t =
    case MP.runParser parser filepath t of
        Left err -> Left $ MP.parseErrorPretty err
        Right x -> Right x

word32Parser :: Parser Word32
word32Parser = do
    a <- fromIntegral <$> MP.anyChar
    b <- fromIntegral <$> MP.anyChar
    c <- fromIntegral <$> MP.anyChar
    d <- fromIntegral <$> MP.anyChar
    pure $ shiftL a 24 + shiftL b 16 + shiftL c 8 + d
