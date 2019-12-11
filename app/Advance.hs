{-# LANGUAGE OverloadedStrings #-}
module Advance
    ( 
        main
    )
    where
import qualified Data.Text as T
import System.FilePath.Posix
import System.Directory
import Data.List (isPrefixOf,isSuffixOf)
import Data.Char (isDigit)


makeTemplate :: String -> String -> Int -> IO()
makeTemplate templateFile outputMask dayNumber = do
        contents <- readFile templateFile
        let output = T.replace  "%%DAY_NUMBER%%" (T.pack $ show dayNumber) (T.pack contents)
        let dayFileName = T.replace "%%DAY_NUMBER%%" (T.pack $ show dayNumber) (T.pack outputMask)
        let outPath = (takeDirectory templateFile) ++ [pathSeparator] ++ (T.unpack dayFileName) 
        writeFile outPath $ T.unpack output
        putStrLn ("file written : " ++ outPath)

getNextNumber path = do
    files <- listDirectory path
    let days = filter (\y -> isSuffixOf ".hs" y && isPrefixOf "Day" y ) files
    let highest = maximum $ map(\y -> (read y)::Int ) $ map (\y ->  filter  isDigit y)  $ days
    return (highest + 1)
    
     

main :: IO ()
main =  do  
    num <- getNextNumber "src"
    makeTemplate "src/TemplateDay.txt" "Day%%DAY_NUMBER%%.hs" num
    makeTemplate "test/TemplateDay.txt" "Day%%DAY_NUMBER%%Spec.hs" num

