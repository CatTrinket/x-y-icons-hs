module GARC (GARC(..)) where

import Control.Monad (liftM3, replicateM, when)
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
    magic <- getLazyByteString 4  -- Always CRAG
    headerSize <- getWord32le'  -- Always 0x0000001C
    byteOrderMark <- getWord16le'  -- Always 0xFEFF
    unknown <- getWord16le'  -- Always 0x0400
    chunks <- getWord32le'  -- Always 0x00000004
    dataOffset <- getWord32le'  -- Start of actual data
    garcLength <- getWord32le'  -- Length of entire GARC
    lastLength <- getWord32le'  -- ????? (this is what I called it in garc.py)

    when (magic /= (BLC.pack "CRAG")) (error "wrong magic in GARC header")
    when (headerSize /= 0x0000001C) (error "wrong header size in GARC header")
    when (byteOrderMark /= 0xFEFF) (error "wrong BOM in GARC header")
    when (unknown /= 0x0400) (error "wrong unknown in GARC header")
    when (chunks /= 0x00000004) (error "wrong chunk count in GARC header")

    return dataOffset  -- This is all that's useful

-- FATO section
getFATO :: Get [Int]
getFATO = do
    magic <- getLazyByteString 4  -- Always OTAF
    fatoLength <- getWord32le'  -- Length of this header + FATB offset list
    offsetCount <- getWord16le'  -- Number of FATB offsets
    padding <- getWord16le'  -- Always 0xFFFF

    when (magic /= (BLC.pack "OTAF")) (error "wrong magic in FATO header")
    when (padding /= 0xFFFF) (error "wrong padding in FATO header")

    replicateM offsetCount getWord32le'  -- FATB offsets

-- FATB section
getFATB :: [Int] -> Get [[(Int, Int, Int)]]
getFATB offsets = do
    magic <- getLazyByteString 4  -- Always BTAF
    fatbLength <- getWord32le'
    fileCount <- getWord32le'

    when (magic /= (BLC.pack "BTAF")) (error "wrong magic in FATB header")

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
    magic <- getLazyByteString 4  -- Always BMIF
    headerSize <- getWord32le'  -- Always 0x0000000C
    fimbLength <- getWord32le'

    when (magic /= (BLC.pack "BMIF")) (error "wrong magic in FIMB header")
    when (headerSize /= 0x0000000C) (error "wrong header size in FIMB header")

    position <- fmap fromIntegral bytesRead
    skip (filesStart - position)

    mapM (mapM lookAhead) (map (map getIndividualFIMB) fileOffsets)

getIndividualFIMB :: (Int, Int, Int) -> Get BL.ByteString
getIndividualFIMB (start, end, length) = do
    skip start
    getLazyByteString (fromIntegral length)
