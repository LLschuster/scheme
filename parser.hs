module Main where 
import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)
import Control.Monad


symbol :: Parser Char 
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

data LispVal = Atom String 
	| List [LispVal]
	| DottedList [LispVal] LispVal
	| Number Integer
	| String String
	| Bool Bool

parseString :: Parser LispVal
parseString = do 
	char '"'
	x <- many (noneOf "\"")
	char '"'
	return $ String x

parseAtom :: Parser LispVal 
parseAtom = do
	first <- letter <|> symbol
	rest <- many (letter <|> symbol <|> digit)
	let atom = first:rest
	return $ case atom of 
		"#t" -> Bool True
		"#f" -> Bool False
		_ -> Atom atom

parseNumber :: Parser LispVal 
--parseNumber = liftM (Number . read) $ many1 digit
parseNumber = do
	num <- read <$> (many1 digit) 
	return $ Number num

parseExpr :: Parser LispVal
parseExpr = parseString <|> parseAtom <|> parseNumber

readExpr :: String -> String 
readExpr input = case parse parseExpr "lisp" input of 
	Left err -> "No match " ++ show err
	Right val -> "Found value"

main :: IO ()
main = do 
	(expr:_) <- getArgs
	putStrLn (readExpr expr)
