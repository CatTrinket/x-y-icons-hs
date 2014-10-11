import Control.Applicative ((<$>))
import Control.Monad (replicateM)
import Data.Binary (Binary, decode, get, put)
import Data.Binary.Get (Get, getWord8, getWord16le, runGet, skip)
import Data.Bits (Bits, (.&.), (.|.), shiftL, shiftR)
import qualified Data.ByteString.Lazy as BL
import Data.Word (Word8)
import GARC (GARC(files))
import NLZSS (decompress)
import System.Environment (getArgs)
import System.FilePath (joinPath)
import Text.Printf (printf)

import Codec.Picture (Image, PixelRGBA8(..), generateImage, writePng)

-- The size the image is stored as, versus its actual intended dimensions.
-- Each icon has a footer at 0x1000 that contains this info, but it's always
-- the same.
rawWidth = 64
rawHeight = 32

width = 40
height = 30

-- A colour; five bits each RGB, one bit alpha
data Colour = Colour Int Int Int Int deriving Show

instance Binary Colour where
    put = undefined
    get = parseColour . fromIntegral <$> getWord16le

parseColour :: Int -> Colour
parseColour colour = Colour r g b a
    where
        r = (colour `shiftR` 11) .&. 0x1F
        g = (colour `shiftR` 6) .&. 0x1F
        b = (colour `shiftR` 1) .&. 0x1F
        a = colour .&. 1

-- A palette
type Palette = [Colour]

getPalette :: Get Palette
getPalette = do
    paletteLength <- fromIntegral <$> getWord16le
    replicateM paletteLength (get :: Get Colour)

-- A pixel, represented as a palette index
type Pixel = Int

-- An icon
data Icon = Icon Palette [Pixel] deriving Show

instance Binary Icon where
    put = undefined

    get = do
        skip 2  -- Unknown; always 0x02 0x00
        palette <- getPalette
        pixels <- getPixels (length palette > 16)
        return (Icon palette pixels)

getPixels :: Bool -> Get [Int]
getPixels True = untile <$> replicateM (rawWidth * rawHeight) getWord8'
getPixels False =
    untile . concatMap splitPixels <$>
    replicateM (rawWidth * rawHeight `div` 2) getWord8'
    where splitPixels byte = [byte `shiftR` 4, byte .&. 0x0F]

getWord8' :: Integral a => Get a
getWord8' = fromIntegral <$> getWord8

untileIndex :: Int -> Int
untileIndex n = tileNum * 64 + withinTile
    where
        (y, x) = n `divMod` rawWidth
        (tileX, subX) = x `divMod` 8
        (tileY, subY) = y `divMod` 8
        tileNum = tileY * (rawWidth `div` 8) + tileX
        withinTile =
            (subX .&. 0x01) .|.
            ((subX .&. 0x02) `shiftL` 1) .|.
            ((subX .&. 0x04) `shiftL` 2) .|.

            ((subY .&. 0x01) `shiftL` 1) .|.
            ((subY .&. 0x02) `shiftL` 2) .|.
            ((subY .&. 0x04) `shiftL` 3)

untile :: [Pixel] -> [Pixel]
untile pixels = map ((pixels !!) . untileIndex) [0 .. rawWidth * rawHeight - 1]

unpackIcons :: BL.ByteString -> [Icon]
unpackIcons = map (runGet (get :: Get Icon)) . map decompress . concat . files . decode

-- Find the icon file, given the ROM dump directory
iconPath :: FilePath -> FilePath
iconPath dir = joinPath [dir, "romfs", "a", "0", "9", "3"]


-- Turn an Icon into something we can feed to JuicyPixels — at the time of
-- writing, JuicyPixels doesn't do palette + transparency
iconToImage :: Icon -> Image PixelRGBA8
iconToImage (Icon palette pixels) = generateImage getPixel width height
    where
        getPixel x y = roundedPalette !! (pixels !! (x + y * rawWidth))
        roundedPalette = map colourToPixel palette

colourToPixel :: Colour -> PixelRGBA8
colourToPixel (Colour r g b a) = PixelRGBA8 r' g' b' a'
    where
        r' = roundChannel r
        g' = roundChannel g
        b' = roundChannel b
        a' = if a == 0 then 0 else 255 :: Word8
        roundChannel x = fromIntegral ((x * 255 + 15) `div` 31) :: Word8

saveIcon :: FilePath -> (Int, Icon) -> IO ()
saveIcon dir (n, icon) = writePng path image
    where
        path = joinPath [dir, filename]
        filename = printf "%d.png" n
        image = iconToImage icon


-- 
main = do
    (romDir:outputDir:_) <- getArgs
    icons <- unpackIcons <$> BL.readFile (iconPath romDir)
    mapM_ (saveIcon outputDir) (zip [0..] icons)
