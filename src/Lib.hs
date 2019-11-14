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

import Numeric
import Data.Char

import System.Environment

someFunc :: IO ()
someFunc = do
  print "iniciando"
  [ arg ] <- getArgs
  image <-  someFunc2 arg
  print $ length $ imageRawObjects $ image
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
  { rawObjectNumSlots :: Word64
  , rawObjectClassIndex :: Word64
  , rawObjectFormat :: Word64
  , rawObjectHash :: Word64
  , rawObjectSlots :: [Word64]
  } deriving (Show)


sliceFromBits offset size bits =
  shiftR bits offset .&. (shift 1 size - 1)

rawObjectParser = do
  -- pos <- fromIntegral @_ @Word64 <$> bytesRead
  pos <- bytesRead
  header <- getWord64le
  let
      numSlots' = sliceFromBits 56 8 header
      format = sliceFromBits 24 5  header
      hash = sliceFromBits 64 22 header
      classIndex = sliceFromBits 0 22 header
  if numSlots' == 255
    then do
      header' <- getWord64le
      let
        numSlots' = sliceFromBits 56 8 header'
        realNumSlots = header .&. 0xffffffffffff
        format = sliceFromBits 24 5  header'
        hash = sliceFromBits 64 22 header'
        classIndex = sliceFromBits 0 22 header'
      when (numSlots' /= 255)
        $ fail "255 no estaba ahi"
      slots <- replicateM (fromIntegral realNumSlots)  getWord64le

      return $ (pos, RawObject realNumSlots classIndex format hash slots)

    else do
      slots <- replicateM (fromIntegral $ max numSlots' 1)  getWord64le

      return $ (pos, RawObject numSlots' classIndex format hash slots)



-- imageParser :: Parser Image
imageParser = do
  version <- getWord32le
  imageHeader <- imageHeaderParser
  -- object <- many' rawObjectParser
  objects <- many rawObjectParser
  -- object <- return []

  return $ Image version imageHeader objects
  -- return $ Image version imageHeader objects
  -- return $ (version, imageHeader, objects)

-- imageHeaderParser :: Parser ImageHeader
imageHeaderParser = do
  imageHeaderSize <- getWord32le -- readWord();
  objectMemorySize <- getWord64le -- readWord(); //first unused location in heap
  oldBaseAddr <- getWord64le -- readWord(); //object memory base address of image
  specialObjectsOopInt <- getWord64le -- readWord(); //oop of array of special oops
  lastHash <- getWord64le -- readWord(); //Should be loaded from, and saved to the image header
  savedHeaderWords <- sequence . replicate 5 $  getWord64le -- [];
  firstSegSize <- getWord64le -- readWord();

  goto $ fromIntegral @_ @Int64 imageHeaderSize

  return (ImageHeader imageHeaderSize objectMemorySize oldBaseAddr specialObjectsOopInt lastHash savedHeaderWords firstSegSize)

goto :: Int64 -> Parser ()
goto n = do
  pos <- bytesRead
  skip $ fromIntegral (n - pos)
