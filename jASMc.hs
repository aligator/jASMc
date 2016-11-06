import System.IO
import qualified Data.List.Split as Split
import qualified Data.Char as Char
import Data.Bits
import Data.Word
--import qualified Data.Binary.Get as Get

data Register = Register {regId :: Integer}  deriving (Show,Eq) 
data Command = Command {cmdId :: Integer} deriving (Show,Eq) 

getRegister :: String -> Register
getRegister "a" = Register 0
getRegister "b" = Register 1
getRegister "c" = Register 2
getRegister "d" = Register 3
getRegister "e" = Register 4
getRegister "f" = Register 5
getRegister "g" = Register 6
getRegister "h" = Register 7
getRegister "w1" = Register 8
getRegister "w2" = Register 9
getRegister "w3" = Register 10
getRegister "w4" = Register 11
getRegister "ax" = Register 12
getRegister "bx" = Register 13
getRegister "rx" = Register 14
getRegister s = Register (-1)

getCommand :: String -> Command
getCommand "Ende" = Command 0
getCommand "I++" = Command 2
getCommand "Print" = Command 8
getCommand s = Command (-1)

isInteger :: String -> Bool
isInteger ""  = False
isInteger "." = False
isInteger xs  =
  case dropWhile Char.isDigit xs of
    ""       -> True
    _        -> False

getRegId :: Register -> Integer
getRegId reg = regId reg

getRegIdByStr :: String -> Integer
getRegIdByStr reg = getRegId (getRegister reg)

getCmdId :: Command -> Integer
getCmdId cmd = cmdId cmd

getCmdIdByStr :: String -> Integer
getCmdIdByStr cmd = getCmdId (getCommand cmd)

splitOnBlanc :: String -> [[Char]]
splitOnBlanc str = Split.splitOn " " str

extractCmdfromSplitted :: [[Char]] -> Integer
extractCmdfromSplitted splitted = getCmdIdByStr (splitted !! 0)

extractCmd :: String -> Integer
extractCmd s = extractCmdfromSplitted (splitOnBlanc s)


checkAsmIsInteger :: String -> Bool
checkAsmIsInteger (x:xs) = x == '#' && isInteger xs

checkAsmIsPointer :: String -> Bool
checkAsmIsPointer = isInteger

checkAsmIsRegister :: String -> Bool
checkAsmIsRegister s = (getRegIdByStr s) >= 0


--encodeWord16toWord8 :: Word16 -> [Word8]
--encodeWord16toWord8 x = map fromIntegral [Get.getByte 1 x, Get.getByte 2 x]

word8toWord32 :: Word8 -> Word32
word8toWord32 x = fromIntegral x

--word32FromWord8 :: Word8 -> Word32
--word32FromWord8 a = ((fromIntegral a) `shiftL` 24 + (fromIntegral 0) `shiftL` 16 + (fromIntegral 0) `shiftL` 8 + (fromIntegral 0))

asmAddCmd :: Word8 -> Word32
asmAddCmd cmd = (word8toWord32 (cmd .&. 63)) `shiftL` 18 

-- boolean true: small Info
-- boolean false: big Info
asmAddInfo :: Word32 -> Word8 -> Bool -> Word32
asmAddInfo bin info false = bin .|. (word8toWord32 ((info .&. 6)) `shiftL` 15)
asmAddInfo bin info true = bin .|. (word8toWord32 ((info .&. 7)) `shiftL` 15)

-- boolean true: small Info
-- boolean false: big Info
asmBuild :: Word8 -> Word8 -> Bool -> Word32
asmBuild cmd info smallInfo = asmAddInfo (asmAddCmd cmd) info smallInfo

-- extractData :: String -> Integer
-- extractData s = extractCmdfromSplitted (splitOnBlanc s)


main = do
    --putStrLn (show(extractCmd "asf sdf asf sdf"))
    putStrLn (show(getCmdIdByStr "I++"))
