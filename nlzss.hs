module NLZSS (getLZSS11) where

import Control.Monad (replicateM, replicateM_, when)
import Data.Binary.Get (Get, getWord8, getWord16be, getWord32be, lookAhead)
import Data.Bits ((.&.), (.|.), shiftL, shiftR, testBit)
import qualified Data.ByteString.Lazy as BL
import Data.Int (Int64)
import Data.Word (Word8)

-- Various extra word-getters
getWord8' = fmap fromIntegral getWord8
getWord16be' = fmap fromIntegral getWord16be
getWord24be' = fmap fromIntegral getWord24be
getWord32be' = fmap fromIntegral getWord32be

getWord24be :: Get Int
getWord24be = do
    a <- getWord8'
    b <- getWord8'
    c <- getWord8'

    return $ (a `shiftL` 16) .|. (b `shiftL` 8) .|. c

getWord24le :: Get Int
getWord24le = do
    a <- getWord8'
    b <- getWord8'
    c <- getWord8'

    return $ a .|. (b `shiftL` 8) .|. (c `shiftL` 16)

-- Parse header and decompress LZSS11
getLZSS11 :: Get BL.ByteString
getLZSS11 = do
    magic <- getWord8
    when (magic /= 0x11) (error "Not LZSS11: Doesn't start with 0x11")

    finalLength <- getWord24le

    if finalLength == 0
    then return BL.empty
    else getLZSS11Bytes finalLength BL.empty 0 0

-- Recursively decompress the actual compressed part of LZSS11
getLZSS11Bytes :: Int -> BL.ByteString -> Word8 -> Int -> Get BL.ByteString
getLZSS11Bytes finalLength soFar flags flagsLeft = do
    -- Read a flag byte if necessary
    (flags, flagsLeft) <-
        if flagsLeft == 0
        then do
            newFlags <- getWord8
            return (newFlags, 8)
        else return (flags, flagsLeft)

    -- Depending on the next flag, either...
    soFar <-
        if flags `testBit` 7
        then do
            -- Append a previously-seen string of bytes
            (count, offset) <- getLZSS11BackRef
            return $ soFar `BL.append` (applyBackref soFar count offset)
        else do
            -- Copy one byte verbatim
            byte <- getWord8
            return (soFar `BL.snoc` byte)

    -- Return our decompressed data if we're done; otherwise recurse
    let lengthSoFar = (fromIntegral . BL.length) soFar

    when (lengthSoFar > finalLength) (error "Somehow we got too long")

    if lengthSoFar == finalLength
    then return soFar
    else getLZSS11Bytes finalLength soFar (flags `shiftL` 1) (flagsLeft - 1)

-- Get a count/offset backref pair
-- We specifically get Int64s because BL.take, BL.drop, and BL.length need them
getLZSS11BackRef :: Get (Int64, Int64)
getLZSS11BackRef = do
    -- 4 bit control
    control <- fmap (`shiftR` 4) (lookAhead getWord8)

    case control of
        0 -> do
            -- 8 bit count, 12 bit offset
            countOffset <- getWord24be' :: Get Int64
            let count = ((countOffset `shiftR` 12) .&. 0xFF) + 0x11
            let offset = (countOffset .&. 0xFFF) + 1

            return (count, offset)
        1 -> do
            -- 16 bit count, 12 bit offset
            countOffset <- getWord32be' :: Get Int64
            let count = ((countOffset `shiftR` 12) .&. 0xFFFF) + 0x111
            let offset = (countOffset .&. 0xFFF) + 1

            return (count, offset)
        n -> do
            -- 4 bit count (instead of control), 12 bit offset
            countOffset <- getWord16be' :: Get Int64
            let count = (fromIntegral n + 1)
            let offset = (countOffset .&. 0xFFF) + 1

            return (count, offset)

-- Copy `count` bytes to the end of the byte string, starting `offset` bytes
-- from the end
applyBackref :: BL.ByteString -> Int64 -> Int64 -> BL.ByteString
applyBackref bytes count offset
    | offset > bytesLength =
        error "Offset larger than so-far-decompressed byte string"
    | otherwise = BL.take count . BL.cycle . takeEnd offset $ bytes
    where
        bytesLength = BL.length bytes
        takeEnd offset bytes = BL.drop (bytesLength - offset) bytes
