module Main where

import Data.List
import Data.List.Split
import System.Console.Terminal.Size
import System.Environment (getArgs)
import System.IO
import System.Process
import System.ShQQ

--qstatOutput = "output.qstat3"

main = do
  args <- getArgs
  a <- readShell "qstat -f | sed '/\\t/d'"
--  a <- readFile qstatOutput
  Just win <- hSize stdout
  if a == "" 
     then putStrLn "\nNo Jobs on Queue !!\n"
     else do
          let mosaicD = map words $ lines a
              widthTerminal = width win :: Int
              transf  = transformer widthTerminal mosaicD
          if args == [] 
             then putStrLn $ unlines transf
             else do
                  let oriz = take widthTerminal $ repeat '-'
                  putStrLn $ unlines $ [oriz] ++ (filter (\x -> elem (head args) (words x)) transf) ++ [oriz]

transformer :: Int -> [[String]] -> [String]
transformer winS xss = let 
   a = map singleLineTransformer xss
   b = filter (/= "lolzzzz") a
   c = splitWhen (== "") b
   d = filter (/= []) c
   e = map unifier d
   in prettyPrinter winS e

singleLineTransformer :: [String] -> String
singleLineTransformer [] = []
singleLineTransformer xs = case head xs of
   "Job"       -> takeWhile (/= '.') $ xs !! 2
   "Job_Name"  -> xs !! 2
   "Job_Owner" -> takeWhile (/= '@') $ xs !! 2 
   "job_state" -> xs !! 2
   "Resource_List.mem" -> xs !! 2
   "Resource_List.nodes" -> dropWhile (== '=') $ dropWhile (/= '=') $ xs !! 2
   "exec_host"  -> takeWhile (/= '/') $ xs !! 2
   "start_time" -> xs!!4 ++ "_" ++ xs!!3 ++ "_" ++ (take 5 (xs!!5))
   otherwise -> "lolzzzz"

unifier :: [String] -> [String]
unifier xs = if length xs == 8 
   then xs 
   else take 4 xs ++ [" "] ++ drop 4 xs ++ [" "]

prettyPrinter winS xs = let 
  lengthUnified = transpose $ map printWellSpacedColumn $ transpose xs
  lengthVsTerminal = adaptToTerminal winS lengthUnified
  addingbars x = "| " ++ (intercalate " | " x) ++ " |"
  verticalBarsAdded    = map addingbars lengthVsTerminal
  lengthOfIt           = length $ head verticalBarsAdded
  orizontalBar         = take lengthOfIt $ repeat '-'
  orizontalBarAdded    = [orizontalBar] ++ verticalBarsAdded ++ [orizontalBar]
  in orizontalBarAdded

printWellSpacedColumn xs = let 
    matchLength n str = if length str == n then str else matchLength n $ " " ++ str 
    maxLength = maximum $ map length xs
    in map (matchLength maxLength) xs

adaptToTerminal winS xs = let
  lengthString = length $ unwords $ head xs
  howManyField = length $ head xs
  realLength   = lengthString + ((howManyField-1)*2+4)
  whiteSpaces  = winS - realLength
  newString    = if whiteSpaces < 0 then adaptToTerminal winS (map shorten xs) else map (reLength whiteSpaces) xs
  in newString

reLength diffL x = take 1 x ++ [concat (take diffL (repeat " ")) ++ (x!!1)] ++ drop 2 x

shorten x = if length (x!!1) > 30 then take 1 x ++ [reverse $ take 30 $ reverse(x!!1)] ++ drop 2 x else init x

