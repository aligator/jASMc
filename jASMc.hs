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

extractCmd :: String -> Word8
extractCmd s = extractCmdfromSplitted (splitOnBlanc s)

--extractOneData pos register integer pointer = 
extractOneData :: Integer -> Bool -> Bool -> Bool -> Word8
extractOneData pos true false false = getRegIdByStr (splitted !! pos)
extractOneData pos false true false = getAsmIntegerByString (splitted !! pos)
extractOneData pos false false true = getAsmPointerByString (splitted !! pos)

extractBigData :: Bool -> Bool -> Bool -> Word16
extractOneData true false false = getRegIdByStr (splitted !! 1)
extractOneData false true false = getAsmIntegerByString (splitted !! 1)
extractOneData false false true = getAsmPointerByString (splitted !! 1)

--Bool: Is2Values
extractData :: String -> Bool -> Word16
extractData s true = asmCombineValues 
extractData s false = extractCmdfromSplitted (splitOnBlanc s)

asmGetInteger :: [[Char]] -> Integer
asmGetInteger (x:xs) = read xs :: Integer

asmGetPointer :: [[Char]] -> Integer
asmGetPointer x = read x :: Integer

checkAsmIs2Values :: [[Char]] -> Bool
checkAsmIs2Values splitted = (length splitted) == 3

checkAsmIsInteger :: String -> Bool
checkAsmIsInteger (x:xs) = x == '#' && isInteger xs

checkAsmIsPointer :: String -> Bool
checkAsmIsPointer = isInteger

checkAsmIsRegister :: String -> Bool
checkAsmIsRegister s = (getRegIdByStr s) >= 0


word8toWord16 :: Word8 -> Word16
word8toWord16 x = fromIntegral x

word8toWord32 :: Word8 -> Word32
word8toWord32 x = fromIntegral x

word16toWord32 :: Word16 -> Word32
word16toWord32 x = fromIntegral x


asmAddCmd :: Word8 -> Word32
asmAddCmd cmd = (word8toWord32 (cmd .&. 63)) `shiftL` 18 

asmAddInfo :: Word32 -> Word8 -> Word32
asmAddInfo bin info = bin .|. (word8toWord32 ((info .&. 7)) `shiftL` 15)

asmAddValue :: Word32 -> Word16 -> Word32
asmAddValue bin value = bin .|. (word16toWord32 ((value .&. 16383)))

asmBuild :: Word8 -> Word8 -> Word16 -> Word32
asmBuild cmd info value = asmAddValue (asmAddInfo (asmAddCmd cmd) info) value

asmBuildFrom2val :: Word8 -> Word8 -> Word8 -> Word8 -> Word32
asmBuildFrom2val cmd info valL valR = asmAddValue (asmAddInfo (asmAddCmd cmd) info) (asmCombineValues valL valR)

asmCombineValues :: Word8 -> Word8 -> Word16
asmCombineValues valL valR = ((word8toWord16 (valL)) `shiftL` 8) .|. word8toWord16 (valR)




main = do
    --putStrLn (show(extractCmd "asf sdf asf sdf"))
    putStrLn (show(getCmdIdByStr "I++"))
