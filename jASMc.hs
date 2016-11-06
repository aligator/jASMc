import System.IO


data RegisterType = RegisterType {regId :: Integer}  deriving (Show,Eq) 

getRegister :: String -> RegisterType
getRegister "a" = RegisterType 0
getRegister "b" = RegisterType 1
getRegister "c" = RegisterType 2
getRegister "d" = RegisterType 3
getRegister "e" = RegisterType 4
getRegister "f" = RegisterType 5
getRegister "g" = RegisterType 6
getRegister "h" = RegisterType 7
getRegister "w1" = RegisterType 8
getRegister "w2" = RegisterType 9
getRegister "w3" = RegisterType 10
getRegister "w4" = RegisterType 11
getRegister "ax" = RegisterType 12
getRegister "bx" = RegisterType 13
getRegister "rx" = RegisterType 14

getRegId :: RegisterType -> Integer
getRegId reg = regId reg

getRegIdByStr :: String -> Integer
getRegIdByStr reg = getRegId (getRegister reg)


main = do
    putStrLn (show(regId (getRegister("a"))))
