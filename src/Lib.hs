{-# LANGUAGE StrictData #-}
{-# LANGUAGE Strict #-}
module Lib
    where

import Control.Monad
import Prelude hiding (take)
import Data.Binary
import Data.Binary.Get
import Data.Binary.Parser
import Control.Applicative
import qualified Data.Binary.Bits.Get as Bit
import Control.Monad.IO.Class
import           Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BL
import Text.Pretty.Simple
import Data.Word
import Data.Bits
import Data.Int
import Data.List (find)

import Numeric
import Data.Char

import System.Environment

someFunc :: IO ()
someFunc = do
  print "iniciando"
  [ arg ] <- getArgs
  image <-  someFunc2 arg
  pPrint $ length $ imageRawObjects $ image
  -- parseTest imageParser file
-- someFunc2 :: IO ()
someFunc2 arg = do
  file <- BL.readFile arg
  pure $ runGet imageParser file

-- withSize :: Int64 -> Parser a -> Parser a
withSize n p = do
  pos <- bytesRead
  coso <- p
  case compare pos n of
    EQ -> return coso
    LT -> do
      skip $ fromIntegral (pos - n)
      return coso
    GT -> do
      fail "ldhwauldhwuali"


data Image = Image
  { imageVersion :: Word32
  , imageHeader :: ImageHeader
  , imageRawObjects :: [(Int64, RawObject)]
  } deriving (Show)

data RawObjectHeader = RawObjectHeader
  { rawObjectHeaderNumSlots :: Word64
  , rawObjectHeaderFormat :: Word64
  , rawObjectHeaderHash :: Word64
  , rawObjectHeaderClassIndex :: Word64
  } deriving (Show)

data ImageHeader = ImageHeader
  { imageHeaderSize :: Word32
  , objectMemorySize :: Word64
  , oldBaseAddr :: Word64
  , specialObjectsOop :: Word64
  , lastHash :: Word64
  , savedHeaderWords :: [Word64]
  , firstSegSize :: Word64
  } deriving (Show)

data RawObject = RawObject
  { rawObjectHeader :: RawObjectHeader
  , rawObjectSlots :: [Word64]
  } deriving (Show)


sliceFromBits offset size bits =
  shiftR bits offset .&. (shift 1 size - 1)

rawObjectHeaderParser = do
  (rawHeader, header) <- rawObjectHeaderParser'

  if rawObjectHeaderNumSlots header == 255
    then do
      (_, nextHeader) <- rawObjectHeaderParser'
      when (rawObjectHeaderNumSlots nextHeader /= 255)
        $ fail "el header no era 255"
      let realNumSlots = rawHeader .&. 0xffffffffffff
      pure $ nextHeader { rawObjectHeaderNumSlots = realNumSlots }

    else do
      pure header


rawObjectHeaderParser' = do
  header <- getWord64le
  let
    numSlots = sliceFromBits 56 8 header
    format = sliceFromBits 24 5  header
    hash = sliceFromBits 64 22 header
    classIndex = sliceFromBits 0 22 header
  pure (header, RawObjectHeader numSlots format hash classIndex)

rawObjectParser = do
  -- pos <- fromIntegral @_ @Word64 <$> bytesRead
  pos <- bytesRead
  header <- rawObjectHeaderParser
  slots <- replicateM (fromIntegral $ max (rawObjectHeaderNumSlots header) 1)  getWord64le
  return $ (pos, RawObject header slots)

-- findOopInsideImage :: Word64 -> Image -> Maybe (Int64, RawObject)
findOopInsideImage offset image =
  let
    realOffset =
      offset
      - (fromIntegral $ oldBaseAddr $ imageHeader image)
      + (fromIntegral $ imageHeaderSize $ imageHeader image)
  in find (\(o, object) -> fromIntegral o == realOffset) $ imageRawObjects image


-- imageParser :: Parser Image
imageParser = do
  version <- getWord32le
  imageHeader <- imageHeaderParser
  objects <- many rawObjectParser

  return $ Image version imageHeader objects

-- imageHeaderParser :: Parser ImageHeader
imageHeaderParser = do
  imageHeaderSize <- getWord32le
  objectMemorySize <- getWord64le -- first unused location in heap
  oldBaseAddr <- getWord64le -- object memory base address of image
  specialObjectsOopInt <- getWord64le -- oop of array of special oops
  lastHash <- getWord64le -- Should be loaded from, and saved to the image header
  savedHeaderWords <- replicateM 5 getWord64le
  firstSegSize <- getWord64le

  goto $ fromIntegral @_ @Int64 imageHeaderSize

  return (ImageHeader imageHeaderSize objectMemorySize oldBaseAddr specialObjectsOopInt lastHash savedHeaderWords firstSegSize)

goto :: Int64 -> Parser ()
goto n = do
  pos <- bytesRead
  skip $ fromIntegral (n - pos)
