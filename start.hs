
module Main where 
import System.Environment

rint :: String -> Int
rint = read

showNumber :: Int -> String
showNumber = show

main :: IO ()
main = do 
	args <- getArgs
	putStrLn ("Your sum is, " ++ showNumber ((rint (args !! 0)) + (rint (args !! 1)))) 
	putStrLn ("Please give me your name")
	name <- getLine
	putStrLn name
