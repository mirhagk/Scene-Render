{- A small library for creating grayscale PNG files.
 - This file is placed into the public domain.
 - Dependencies: Zlib.
 -}

module PNG where

import Data.Array
import Data.Bits
import Data.List
import Data.Word
import qualified Codec.Compression.Zlib as Z
import qualified Data.ByteString.Lazy as B

be8 :: Word8 -> B.ByteString
be8 x = B.singleton x
 
be32 :: Word32 -> B.ByteString
be32 x = B.pack [fromIntegral (x `shiftR` sh) | sh <- [24,16,8,0]]
 
pack :: String -> B.ByteString
pack xs = B.pack $ map (fromIntegral.fromEnum) xs
 
hdr, iHDR, iDAT, iEND :: B.ByteString
hdr = pack "\137\80\78\71\13\10\26\10"
iHDR = pack "IHDR"
iDAT = pack "IDAT"
iEND = pack "IEND"
 
chunk :: B.ByteString -> B.ByteString -> [B.ByteString]
chunk tag xs = [be32 (fromIntegral $ B.length xs), dat, be32 (crc dat)]
    where dat = B.append tag xs

white, black :: Int
white = 255
black = 0

-- | Produces a single grayscale bit given a percent black
gray :: Int -> Int
gray percent = 255 - floor (fromIntegral percent * 2.55)

-- | Return a grayscale PNG file from a two dimensional bitmap stored in a list
-- of lines represented as a list of 0-255 integer values.
png :: [[Int]] -> B.ByteString
png dat = B.concat $ hdr : concat [ihdr, imgdat, iend]
     where height = fromIntegral $ length dat
           width = fromIntegral $ length (head dat)
           ihdr = chunk iHDR $ B.concat 
                     [ be32 width
                     , be32 height
                     , be8 8   -- bits per pixel
                     , be8 0   -- color type
                     , be8 0   -- compression method
                     , be8 0   -- filter method
                     , be8 0 ] -- interlace method
           imgdat = chunk iDAT (Z.compress imgbits)
           imgbits = B.concat $ map scanline dat
           iend = chunk iEND B.empty

writePNG :: FilePath -> [[Int]] -> IO()
writePNG path dat = B.writeFile path $ png dat

writePng :: FilePath -> Array Int Int -> IO ()
writePng path arr = B.writeFile path $ png dat
      where
         dat = tolist2D w  $ take (w*h) (drop s (elems arr))
         s   = arr ! 0
         w   = arr ! 1
         h   = arr ! 2

tolist2D :: Int -> [Int] -> [[Int]]
tolist2D w dat | null (drop w dat) = [take w dat]
               | otherwise         = [take w dat] ++ tolist2D w (drop w dat)
 
scanline :: [Int] -> B.ByteString
scanline dat = B.pack (0 : map fromIntegral dat)
 
bitpack :: [Int] -> B.ByteString
bitpack = B.pack . map fromIntegral
 
crc :: B.ByteString -> Word32
crc xs = updateCrc 0xffffffff xs `xor` 0xffffffff
 
updateCrc :: Word32 -> B.ByteString -> Word32
updateCrc = B.foldl' crcStep
 
crcStep :: Word32 -> Word8 -> Word32
crcStep crc ch = (crcTab ! n) `xor` (crc `shiftR` 8)
    where n = fromIntegral (crc `xor` fromIntegral ch)
 
crcTab :: Array Word8 Word32
crcTab = listArray (0,255) $ flip map [0..255] (\n ->
    foldl' (\c k -> if c .&. 1 == 1
                      then 0xedb88320 `xor` (c `shiftR` 1)
                      else c `shiftR` 1) n [0..7])
