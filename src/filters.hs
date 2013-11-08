
module Filters where

import qualified Data.ByteString as B

import Data.Bits
import Data.Word

unhex :: [Word8] -> [Word8]
unhex [] = []
unhex (_:[]) = error "invalid input length"
unhex (x:y:xs) = (shiftL (hex2val x) 4 .|. hex2val y) : unhex xs
    where hex2val x' = hex2val' (x' .|. 0x20) -- lowercase char
          hex2val' x'
                 | x' >= 48 && x' < 48 + 10 =  0 + x' - 48 -- 48 is '0' in ascii
                 | x' >= 97 && x' < 97 +  6 = 10 + x' - 97 -- 97 is 'a' in ascii
                 | otherwise              = error "invalid hex input"

hex2ascii :: B.ByteString -> B.ByteString
hex2ascii x = B.pack $ unhex $ B.unpack x

-- test
--main = do
--    input <- B.getContents
--    B.putStrLn $ hex2ascii input
