import qualified Data.ByteString.Lazy as BL

import NLZSS (decompress)

main = do
    testLZSS <- decompress `fmap` BL.readFile "testlzss.bin"
    testTarget <- BL.readFile "testtarget.bin"

    print (testLZSS == testTarget)
