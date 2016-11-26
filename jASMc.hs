import System.IO
import System.Environment(getArgs)
import Control.Exception(assert)
import qualified Data.List.Split as Split
import qualified Data.Char as Char
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import Data.Bits
import Data.Word
import Numeric

--import qualified Data.Binary.Get as Get

data Register = Register {regId :: Integer}  deriving (Show,Eq) 
data Command = Command {cmdId :: Integer} deriving (Show,Eq) 
data DataType = INT | REG | POINT deriving (Show, Eq)
data Parameter = Parameter {d :: Integer, dType :: DataType} deriving (Show, Eq)

-- one comand in bytes (5 bytes)
data AsmCmd = AsmCmd {
      b1 :: Word8,
      b2 :: Word8,
      b3 :: Word8,
      b4 :: Word8,
      b5 :: Word8
} deriving (Show)

-- list of commands
data AsmData = AsmData {
      asmData :: [AsmCmd]
} deriving (Show)

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
getCommand "EXIT" = Command 0
getCommand "MOV" = Command 1
getCommand "MOVDW" = Command 2
getCommand "I++" = Command 3
getCommand "I--" = Command 4
getCommand "PRINT" = Command 8
getCommand s = Command (-1)

isInteger :: String -> Bool
isInteger ""  = False
isInteger "." = False
isInteger (x:xs)  = (Char.isDigit x || x == '-') &&
  case dropWhile Char.isDigit xs of
    ""       -> True
    _        -> False
    
isPositiveInteger :: String -> Bool
isPositiveInteger ""  = False
isPositiveInteger "." = False
isPositiveInteger xs  = 
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
extractCmd s = integerToWord8 (extractCmdfromSplitted (splitOnBlanc s))

-- splitted(cmd1, cmd2) twoVal register integer pointer
asmExtractData_ :: [[Char]] -> Bool -> Bool -> Bool -> Bool -> [Integer]
asmExtractData_ splitted False False False True = [asmGetPointer (splitted !! 0)]
asmExtractData_ splitted False False True False = [asmGetInteger (splitted !! 0)]
asmExtractData_ splitted False True False False = [asmGetRegister (splitted !! 0)]
asmExtractData_ splitted True False False True = [asmGetPointer (splitted !! 0),  (asmExtractData (splitted)) !! 0] 
asmExtractData_ splitted True False True False = [asmGetInteger (splitted !! 0), (asmExtractData (splitted)) !! 0]
asmExtractData_ splitted True True False False = [asmGetRegister (splitted !! 0), (asmExtractData (splitted)) !! 0]
asmExtractData_ splitted two reg integer pointer = error ("asmExtractData: ungültige Kombination: data1: " ++ splitted!!0 ++ " data2: " ++ splitted!!1 ++ " two: " ++ show two ++ " reg: " ++ show reg ++ " int: " ++ show integer ++ " pointer: " ++ show pointer)

--              komplette Zeile
asmExtractData :: [[Char]] -> [Integer]
asmExtractData (x:xs) = asmExtractData_ xs (checkAsmIs2Values xs False) (checkAsmIsRegister (xs!!0)) (checkAsmIsInteger (xs!!0)) (checkAsmIsPointer (xs!!0))

asmGetDataType :: String -> DataType
asmGetDataType s 
  | ((checkAsmIsInteger s) == True) = INT
  | ((checkAsmIsPointer s) == True) = POINT
  | ((checkAsmIsRegister s) == True) = REG

asmGetData :: [[Char]] -> [Parameter]
asmGetData x
  | ((checkAsmIs2Values x True) == True) = [Parameter ((asmExtractData x)!!0) (asmGetDataType (x!!1)), Parameter ((asmExtractData x)!!1) (asmGetDataType (x!!2))]
  | ((checkAsmIs2Values x True) == False) = [Parameter ((asmExtractData x)!!0) (asmGetDataType (x!!1))]

-- get values
asmGetRegister :: String -> Integer
asmGetRegister = getRegIdByStr

asmGetInteger :: String -> Integer
asmGetInteger (x:xs) = read xs :: Integer

asmGetPointer :: String -> Integer
asmGetPointer x = read x :: Integer


-- check

                                --with cmd (true) or only parameters (false)
checkAsmIs2Values :: [[Char]] -> Bool -> Bool
checkAsmIs2Values splitted True = (length splitted) == 3
checkAsmIs2Values splitted False = (length splitted) == 2

checkAsmIsInteger :: String -> Bool
checkAsmIsInteger (x:xs) = x == '#' && isInteger xs

checkAsmIsPointer :: String -> Bool
checkAsmIsPointer = isPositiveInteger

checkAsmIsRegister :: String -> Bool
checkAsmIsRegister s = (getRegIdByStr s) >= 0


-- byte-length-convert
word8toWord16 :: Word8 -> Word16
word8toWord16 x = fromIntegral x

word8toWord32 :: Word8 -> Word32
word8toWord32 x = fromIntegral x

word16toWord32 :: Word16 -> Word32
word16toWord32 x = fromIntegral x

word8toWord64 :: Word8 -> Word64
word8toWord64 x = fromIntegral x

word16toWord64 :: Word16 -> Word64
word16toWord64 x = fromIntegral x

word32toWord64 :: Word32 -> Word64
word32toWord64 x = fromIntegral x

word64toWord8 :: Word64 -> Word8
word64toWord8 x = fromIntegral x

integerToWord8 :: Integer -> Word8
integerToWord8 x = fromIntegral x

integerToWord16 :: Integer -> Word16
integerToWord16 x = fromIntegral x

integerToWord32 :: Integer -> Word32
integerToWord32 x = fromIntegral x

-- Byte an der Position pos (0-7) in Word64 w
getByteFromPos :: Word64 -> Int -> Word8
getByteFromPos w pos = word64toWord8 (w `shiftR` (pos * 8))

getAsmCmd :: Word64 -> AsmCmd
getAsmCmd cmd = AsmCmd (getByteFromPos cmd 4) (getByteFromPos cmd 3) (getByteFromPos cmd 2) (getByteFromPos cmd 1) (getByteFromPos cmd 0)

printBin :: (Show a, Integral a) => a -> String
printBin x = showIntAtBase 2 Char.intToDigit x ""

getAsm1 :: (Word64, Bool, Word64) -> Word64
getAsm1 (x, y, z) = x

printAsm :: (Word64, Bool, Word64) -> String
printAsm (x, True, z) = (printBin x) ++ (printBin z)
printAsm (x, False, z) = printBin x

-- Building the Command-Bytes

asmAddCmd :: Word8 -> Word64
asmAddCmd cmd = (word8toWord64 (cmd .&. 63)) `shiftL` 34

asmAddInfo :: Word64 -> Word8 -> Word64
asmAddInfo bin info = bin .|. (word8toWord64 ((info .&. 3)) `shiftL` 32)

asmAddValue :: Word64 -> Word32 -> Word64
asmAddValue bin value = bin .|. (word32toWord64 (value))

asmCombineValues :: Word16 -> Word16 -> Word32
asmCombineValues valL valR = ((word16toWord32 (valL)) `shiftL` 16) .|. word16toWord32 (valR)

asmBuild :: Word8 -> Word8 -> Word32 -> Word64
asmBuild cmd info value = asmAddValue (asmAddInfo (asmAddCmd cmd) info) value

asmBuildFrom2val :: Word8 -> Word8 -> Word16 -> Word16 -> Word64
asmBuildFrom2val cmd info valL valR = asmBuild cmd info (asmCombineValues valL valR)


-- alles zusammenbaun:
--              Bool: 2 Parameter?
asm__ :: Word8 -> Bool -> [Parameter] -> Word64
asm__ cmd is2 cmdData 
   | (cmd == (extractCmd "EXIT")) = asmBuild cmd 0 0

   | (cmd == (extractCmd "MOV") && is2 && ((dType (cmdData!!0)) == INT) && ((dType (cmdData!!1)) == REG)) = asmBuildFrom2val cmd 0 (integerToWord16 (d (cmdData!!0))) (integerToWord16 (d (cmdData!!1)))
   | (cmd == (extractCmd "MOV") && is2 && ((dType (cmdData!!0)) == REG) && ((dType (cmdData!!1)) == POINT)) = asmBuildFrom2val cmd 1 (integerToWord16 (d (cmdData!!0))) (integerToWord16 (d (cmdData!!1)))
   | (cmd == (extractCmd "MOV") && is2 && ((dType (cmdData!!0)) == POINT) && ((dType (cmdData!!1)) == REG)) = asmBuildFrom2val cmd 2 (integerToWord16 (d (cmdData!!0))) (integerToWord16 (d (cmdData!!1)))
   | (cmd == (extractCmd "MOV") && is2 && ((dType (cmdData!!0)) == REG) && ((dType (cmdData!!1)) == REG)) = asmBuildFrom2val cmd 3 (integerToWord16 (d (cmdData!!0))) (integerToWord16 (d (cmdData!!1)))

   | (cmd == (extractCmd "MOVDW") && is2 && ((dType (cmdData!!0)) == INT) && ((dType (cmdData!!1)) == REG) && (d (cmdData!!1)) == (getRegIdByStr "ax")) = asmBuild cmd 0 (integerToWord32 (d (cmdData!!1)))
   | (cmd == (extractCmd "MOVDW") && is2 && ((dType (cmdData!!0)) == INT) && ((dType (cmdData!!1)) == REG) && (d (cmdData!!1)) == (getRegIdByStr "bx")) = asmBuild cmd 1 (integerToWord32 (d (cmdData!!1)))

   | (cmd == (extractCmd "I++") && is2 && ((dType (cmdData!!0)) == REG) && ((dType (cmdData!!1)) == REG)) = asmBuildFrom2val cmd 0 (integerToWord16 (d (cmdData!!0))) (integerToWord16 (d (cmdData!!1)))

   | (cmd == (extractCmd "I--") && is2 && ((dType (cmdData!!0)) == REG) && ((dType (cmdData!!1)) == REG)) = asmBuildFrom2val cmd 0 (integerToWord16 (d (cmdData!!0))) (integerToWord16 (d (cmdData!!1)))

   | (cmd == (extractCmd "PRINT") && (not is2) && ((dType (cmdData!!0)) == INT)) = asmBuild cmd 0 (integerToWord32 (d (cmdData!!0)))
   | (cmd == (extractCmd "PRINT") && (not is2) && ((dType (cmdData!!0)) == REG)) = asmBuild cmd 1 (integerToWord32 (d (cmdData!!0)))
   | (cmd == (extractCmd "PRINT") && (not is2) && ((dType (cmdData!!0)) == POINT)) = asmBuild cmd 2 (integerToWord32 (d (cmdData!!0)))
   | True = error ("CMD: " ++ (show cmd) ++ " two: " ++ (show is2) ++ " data " ++ (show cmdData))




-- asm_ (integerToWord8 (getCmdIdByStr "Mov")) True (asmGetData ["Mov", "#123", "a"])
-- 		CMD1 doppelCmd? CMD2
asm_ :: [[Char]] -> (Word64, Bool, Word64)
asm_ input = (asm__ (integerToWord8 (getCmdIdByStr (input!!0))) (checkAsmIs2Values input True) (asmGetData input), False, 0)

asm :: String -> (Word64, Bool, Word64)
asm input = asm_ (splitOnBlanc input)

asmCmdList2asmData :: [AsmCmd] -> AsmData
asmCmdList2asmData x = AsmData x

asmCmd2ByteList :: AsmCmd -> [Word8]
asmCmd2ByteList x = [b1 x, b2 x, b3 x, b4 x, b5 x]



twoDimensionsTo1_ :: [[a]] -> [a] -> [a]
twoDimensionsTo1_ [] y = y
twoDimensionsTo1_ (x:xs) y = twoDimensionsTo1_ xs (y ++ x)

twoDimensionsTo1 :: [[a]] -> [a]
twoDimensionsTo1 x = twoDimensionsTo1_ x []


asmDataToWord8List :: AsmData -> [Word8]
asmDataToWord8List d = twoDimensionsTo1 (map asmCmd2ByteList (asmData d))

asmDataToByteString :: [Word8] -> String -> B.ByteString
asmDataToByteString d p =
    B.concat [
        BC.pack p
      , B.pack (map fromIntegral d)
    ]

main :: IO ()
main = do
  args <- getArgs
  content <- readFile (args !! 0)
  let asmLines = lines content

  let prAsm s = printAsm (asm s)

  let getAsmCmdFromString s = getAsmCmd (getAsm1 (asm s))
 
  let asmData = asmCmdList2asmData (map getAsmCmdFromString asmLines)
------------------------------------------------------------------
 --putStrLn(show(map prAsm asmLines))
  --putStrLn(show(asmData))
  --putStrLn(show(asmDataToWord8List asmData))

 -- putStrLn(show(asmDataToByteString (asmDataToWord8List asmData) ""))
 
----------------------------------------------
  B.writeFile (args!!1) (asmDataToByteString (asmDataToWord8List asmData) "jASM1")
--main = do
--  putStrLn "jASM-Befehl:"
--  line <- getLine
--  putStrLn (show (asm line))
--  putStrLn (printAsm (asm line))

  --main
