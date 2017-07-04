module Main where

import qualified Data.ByteString.Lazy as BL
import Data.Word
import System.Environment
import Data.Char
import Numeric
import Codec.Binary.UTF8.String
import Data.List
import qualified Numeric as N


showHexAligned :: (Integral a,Show a) => Char -> Int -> a -> String
showHexAligned fill width value = reverse.take width.reverse $ replicate width fill ++ N.showHex value ""

showHex0 = showHexAligned '0'


readHexStr :: (Integral a) => String -> [a]
readHexStr [] = []
readHexStr (t:xs)
        | isSpace t = readHexStr xs
readHexStr (x:y:xs) = fst (head (N.readHex [x,y])) : readHexStr xs

main :: IO ()
main = do
    args <- getArgs
    stream <- BL.getContents
    BL.putStr.BL.pack.parse args.convertStream Reset (-1) $ BL.unpack stream

parse [] = xxd 16

parse [arg] = xxd cols
            where cols = read arg :: Int

data Colours =  Colour Word8
                | Reset


convertStream :: Colours -> Int -> [Word8] -> [(Word8, Colours)]
convertStream _ _ [] = []
convertStream _ _ (255:1:254:127:count:colour:xs) =
            convertStream (Colour colour) (fromIntegral count) xs
convertStream _ 0 (x:xs) =
            (x, Reset) : convertStream Reset (-1) xs
convertStream colour count (x:xs) =
            (x, colour) : convertStream colour (count -1) xs

xxd :: Int ->[(Word8, Colours)] -> [Word8]
xxd cols x = xxdFull cols $ zip [0..] x

xxdFull :: Int -> [(Int,(Word8,Colours))] -> [Word8]
xxdFull _ [] = []
xxdFull cols x = xxdLine line ++ xxdFull cols rest
        where (line, rest) = splitAt cols x

xxdLine :: [(Int,(Word8,Colours))] -> [Word8]
xxdLine [] = fromColour Reset ++ endLine
xxdLine l@((offset,(_,_)):_) =
        index_text ++ encode ": " ++ hexy ++ texty ++ ending
        where
            ending = endLine ++ fromColour Reset
            index_text = encode $ showHex0 8 (fromIntegral offset)
            hexy = concatMap (\(ofs, (ch, col)) -> evenSpace (fromIntegral ofs) ++ fromColour col ++ toHex (fromIntegral ch)) l ++ encode "  "
            texty = concatMap (\(_, (ch, col)) -> fromColour col ++ [printChar ch]) l
            printChar x
                | 0x20 <= x && x < 0x80 = x
                | otherwise = fromIntegral $ ord '~'
            toHex = encode.showHex0 2
            evenSpace i
                    | i `mod` 2 == 0 = [32]
                    | otherwise = []


endLine = encode "\n"
fromColour Reset = [27,0x5b,0x30,0x6d]
fromColour (Colour col) = [27,0x5b,0x31,0x3b,0x33,col,0x6d]
