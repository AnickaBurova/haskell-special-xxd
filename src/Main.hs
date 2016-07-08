module Main where

import qualified Data.ByteString.Lazy as BL
import Data.Word
import System.Environment
import Data.Char
import Numeric
import Codec.Binary.UTF8.String
import Data.List
import Anca.Pipe
import qualified Anca.Text as AT



main :: IO ()
main = do
    args <- getArgs
    stream <- BL.getContents
    stream |> BL.unpack |> convertStream Reset (-1) |> parse args |> BL.pack |> BL.putStr

parse [] = xxd 16
   
parse [arg] = xxd cols
            where cols = (read arg) :: Int

data Colours =  Colour Word8
                | Reset
 

convertStream :: Colours -> Int -> [Word8] -> [(Word8, Colours)]
convertStream _ _ [] = []
convertStream _ _ (255:1:254:127:count:colour:xs) =
            convertStream (Colour colour) (fromIntegral count) xs
convertStream _ 0 (x:xs) =
            (x, Reset) : convertStream (Reset) (-1) xs
convertStream colour count (x:xs) =
            (x, colour) : convertStream colour (count -1) xs
        
xxd :: Int ->[(Word8, Colours)] -> [Word8]
xxd cols x = ( x |> zip [0..] |> xxd_full cols) 

xxd_full :: Int -> [(Int,(Word8,Colours))] -> [Word8]
xxd_full _ [] = []
xxd_full cols x = xxd_line line ++ xxd_full cols rest
        where (line, rest) = splitAt cols x
    
xxd_line :: [(Int,(Word8,Colours))] -> [Word8]
xxd_line [] = "\n" |> encode ++ (fromColour Reset)
xxd_line l@((offset,(_,_)):_) =
        ((AT.showHex0 8 (fromIntegral offset)  ++ ": ") |> encode) ++ hexy ++ [32,32] ++ texty ++ ("\n" |> encode) ++ (fromColour Reset)
        where   
            hexy = l |> map (\(ofs,(ch,col)) -> evenSpace (fromIntegral ofs) ++ (fromColour col) ++(toHex (fromIntegral ch))) |> concat
            texty = l |> map (\(_,(ch,col)) -> (fromColour col) ++ [printChar ch]) |> concat
            printChar x
                | 0x20 <= x && x < 0x80 = x
                | otherwise = ord '~' |> fromIntegral
            toHex = encode.AT.showHex0 2
            evenSpace i 
                    | i `mod` 2 == 0 = [32]
                    | otherwise = []
            

fromColour Reset = [27,0x5b,0x30,0x6d]
fromColour (Colour col) = [27,0x5b,0x31,0x3b,0x33,col,0x6d]
