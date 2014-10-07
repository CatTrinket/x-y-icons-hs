import Data.Binary (decodeFile)
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BLC
import Data.List (unfoldr)
import GARC (GARC(..))
import System.Environment (getArgs)
import Text.Printf (printf)

-- Stuff for pretty-printing the contents of a GARC
-- Hexdump all of a GARC's files' subfiles, each with a "### FILE a.b" header
prettyGARC :: GARC -> String
prettyGARC (GARC files) = do
    (num, file) <- zip ([0..] :: [Int]) files
    (subNum, subFile) <- zip ([0..] :: [Int]) file

    printf "### FILE %d.%d\n%s\n" num subNum (hexDump subFile)

-- Build an actual hexdump; match xxd -u
hexDump :: BL.ByteString -> String
hexDump bytes = do
    (lineNum, line) <- lines16 bytes
    printf "%07X: %-39s  %s\n" lineNum (hexDumpBytes line) (hexDumpASCII line)

-- Split a byte string into numbered sixteen-byte lines
lines16 :: BL.ByteString -> [(Int, BL.ByteString)]
lines16 = zip [0x0, 0x10..] . unfoldr line
    where
        line bytes | BL.null bytes = Nothing
        line bytes = Just (BL.splitAt 16 bytes)

-- Build the hex part of a hexdump line
hexDumpBytes :: BL.ByteString -> String
hexDumpBytes = unwords . unfoldr col . (>>= printf "%02X") . BL.unpack
    where
        col "" = Nothing
        col hexDigits = Just (splitAt 4 hexDigits)

-- Build the ASCII part of a hexdump line
hexDumpASCII :: BL.ByteString -> String
hexDumpASCII = map (\c -> if ' ' <= c && c <= '~' then c else '.') . BLC.unpack


-- From the command line: take filenames as args; read and pretty-print GARCs
main = do
    filenames <- getArgs
    garcs <- mapM decodeFile filenames
    mapM_ (putStrLn . prettyGARC) garcs
