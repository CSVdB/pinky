module MNIST.Parser.Internal where

import Import

import Neural

import MNIST.DataSet
import Utils.Parser

import Text.Megaparsec.Byte

type Pixel = Double

pixelParser :: Parser Pixel
pixelParser = (/ 256) . fromIntegral <$> anyChar

imageParser :: Parser (S Image)
imageParser = do
    pixels <- replicateM (nOfRows * nOfCols) pixelParser
    maybe (fail "Can't parse image") pure $ listToS pixels

labelParser :: Parser (S Label)
labelParser = do
    c <- anyChar
    case intToS $ fromIntegral c of
        Nothing -> fail "label > 9"
        Just x -> pure x
