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
xxd cols x = --((show cols ) ++ "   "++ (x |> length |> show))|> encode
         ( x |> zip [0..] |> xxd_full cols) 

xxd_full :: Int -> [(Int,(Word8,Colours))] -> [Word8]
xxd_full _ [] = []
xxd_full cols x = xxd_line line ++ xxd_full cols rest
        where (line, rest) = splitAt cols x
    
xxd_line :: [(Int,(Word8,Colours))] -> [Word8]
xxd_line [] = "\n" |> encode
xxd_line l@((offset,(_,_)):_) =
        ((AT.showHex0 8 (fromIntegral offset)  ++ ": ") |> encode) ++ hexy ++ [32,32] ++ texty ++ ("\n" |> encode)
        where   
            hexy = l |> map (\(ofs,(ch,col)) -> evenSpace (fromIntegral ofs) ++ (toHex (fromIntegral ch))) |> concat
            texty = l |> map (\(_,(ch,col)) -> ch) |> map printChar
            printChar x
                | 0x20 <= x && x < 0x80 = x
                | otherwise = ord '~' |> fromIntegral
            toHex = encode.AT.showHex0 2
            evenSpace i 
                    | i `mod` 2 == 0 = [32]
                    | otherwise = []
            






-- xxd _ _ _ _ [] = []
-- xxd i todo cols colour x 
    -- | todo > cols = encode ((AT.showHex0 8 i) ++ ": ") ++ xxd i (todo - 1) cols colour x
-- xxd i 0 cols colour x = encode ("\n") ++ xxd i (cols + 1) cols colour x
-- xxd i todo cols colour (x:xs) = encode (AT.showHex0 2 (fromIntegral x)) ++ xxd (i+1) (todo - 1) cols colour xs




