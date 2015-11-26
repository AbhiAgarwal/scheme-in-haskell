import System.Environment
import Control.Monad
import Text.ParserCombinators.Parsec hiding (spaces)

-- Parser that recognizes any number of whitespace characters
spaces :: Parser ()
spaces = skipMany1 space

-- recognizes a single one of any of the characters in the string passed to it
symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

-- Algebraic data type
--  Defines a set of possible values that a variable of type LispVal can hold:
--      1. An Atom, which stores a String naming the atom
--      2. A List, which stores a list of other LispVals (Haskell lists are denoted by brackets); also called a proper list.
--      3. A DottedList, representing the Scheme form (a b . c); also called an improper list. This stores a list of all elements but the last, and then stores the last element as another field
--      4. A Number, containing a Haskell Integer
--      5. A String, containing a Haskell String
--      6. A Bool, containing a Haskell boolean value

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

-- <|>: choice operator - tries the first parser, then if it fails, tries the second.
parseAtom :: Parser LispVal
parseAtom = do
    first <- letter <|> symbol
    rest <- many (letter <|> digit <|> symbol)
    let atom = first:rest
    return $ case atom of
        "#t" -> Bool True
        "#f" -> Bool False
        _    -> Atom atom

parseNumber :: Parser LispVal
parseNumber = liftM (Number . read) $ many1 digit

parseList :: Parser LispVal
parseList = liftM List $ sepBy parseExpr spaces

parseDottedList :: Parser LispVal
parseDottedList = do
    head <- endBy parseExpr spaces
    tail <- char '.' >> spaces >> parseExpr
    return $ DottedList head tail

parseQuoted :: Parser LispVal
parseQuoted = do
    char '\''
    x <- parseExpr
    return $ List [Atom "quote", x]

parseExpr :: Parser LispVal
parseExpr = parseAtom
    <|> parseString
    <|> parseNumber
    <|> parseQuoted
    <|> do
            char '('
            x <- try parseList <|> parseDottedList
            char ')'
            return x    

-- We name the parameter input, and pass it, along with the symbol parser we defined above to the Parsec function parse.
-- The second parameter to parse is a name for the input.
-- Parse can return either the parsed value or an error, so we need to handle the error case.
--  Following typical Haskell convention, Parsec returns an Either data type:
--      - The Left constructor to indicate an error.
--      - the Right one for a normal value.
-- We use a case...of construction to match the result of parse against these alternatives.
-- Bind to spaces: attempt to match the first parser, then attempt to match the second with the remaining input, and fail if either fails.
readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
    Left err -> "No match: " ++ show err
    Right val -> "Found value"

main :: IO ()
main = do
    (expr:_) <- getArgs
    putStrLn (readExpr expr)
