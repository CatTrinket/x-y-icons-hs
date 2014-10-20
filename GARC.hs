module GARC (GARC(..)) where

import Control.Monad (liftM3, replicateM)
import Data.Binary (Binary, Get, get, put)
import Data.Binary.Get (bytesRead, getLazyByteString, getWord16le, getWord32le,
    lookAhead, skip)
import Data.Bits (popCount)
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BLC


-- Include fromIntegral in Word-getters to avoid littering it everywhere
getWord16le' = fmap fromIntegral getWord16le
getWord32le' = fmap fromIntegral getWord32le

-- GARC
data GARC = GARC { files :: [[BL.ByteString]] } deriving (Show)

instance Binary GARC where
    put = undefined

    get = do
        dataOffset <- getGARCHeader
        fatbOffsets <- getFATO
        fileOffsets <- getFATB fatbOffsets
        files <- getFIMB dataOffset fileOffsets

        return (GARC files)

-- GARC header
getGARCHeader :: Get Int
getGARCHeader = do
    "CRAG" <- BLC.unpack `fmap` getLazyByteString 4  -- "Magic bytes"
    0x0000001C <- getWord32le  -- Header length
    0xFEFF <- getWord16le  -- Byte order mark
    0x0400 <- getWord16le  -- Unknown
    0x00000004 <- getWord32le  -- Chunk count
    dataOffset <- getWord32le'  -- Start of actual data
    skip 4  -- Length of entire GARC
    skip 4  -- Unknown (called "last_length" in Python version)

    return dataOffset  -- This is all that's useful

-- FATO section
getFATO :: Get [Int]
getFATO = do
    "OTAF" <- BLC.unpack `fmap` getLazyByteString 4  -- "Magic bytes"
    skip 4  -- Length of this header + FATB offset list
    offsetCount <- getWord16le'  -- Number of FATB offsets
    0xFFFF <- getWord16le  -- Padding

    replicateM offsetCount getWord32le'  -- FATB offsets

-- FATB section
getFATB :: [Int] -> Get [[(Int, Int, Int)]]
getFATB offsets = do
    "BTAF" <- BLC.unpack `fmap` getLazyByteString 4  -- "Magic bytes"
    fatbLength <- getWord32le'
    skip 4  -- File count

    fileOffsets <- mapM lookAhead (map getIndividualFATB offsets)

    skip (fatbLength - 12)

    return fileOffsets

getIndividualFATB :: Int -> Get [(Int, Int, Int)]
getIndividualFATB offset = do
    skip offset

    -- Read 32-bit bitfield; get (start, end, length) for each bit set
    -- (Ignoring *which* bits are set — 011 and 101 are both just "two bits")
    fileCount <- fmap popCount getWord32le'
    replicateM fileCount (liftM3 (,,) getWord32le' getWord32le' getWord32le')

-- FIMB section
getFIMB :: Int -> [[(Int, Int, Int)]] -> Get [[BL.ByteString]]
getFIMB filesStart fileOffsets = do
    "BMIF" <- BLC.unpack `fmap` getLazyByteString 4  -- "Magic bytes"
    0x0000000C <- getWord32le  -- Header size
    skip 4  -- FIMB length

    position <- fromIntegral `fmap` bytesRead
    skip (filesStart - position)

    mapM (mapM lookAhead) (map (map getIndividualFIMB) fileOffsets)

getIndividualFIMB :: (Int, Int, Int) -> Get BL.ByteString
getIndividualFIMB (start, end, length) = do
    skip start
    getLazyByteString (fromIntegral length)
