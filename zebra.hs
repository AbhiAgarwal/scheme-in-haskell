import System.Environment
import Control.Monad
import Control.Monad.Error
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

-- This is an example of point-free style: writing definitions purely in terms of function composition and partial application, without regard to individual values or "points".
unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

-- Evaluator using Pattern Matching
-- The unwordsList function works like the Haskell Prelude's unwords function, which glues together a list of words with spaces.
showVal :: LispVal -> String
showVal (String contents) = "\"" ++ contents ++ "\""
showVal (Atom name) = name
showVal (Number contents) = show contents
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (List contents) = "(" ++ unwordsList contents ++ ")"
showVal (DottedList head tail) = "(" ++ unwordsList head ++ " . " ++ showVal tail ++ ")"

instance Show LispVal where show = showVal

-- Evaluator: adding basic primitives

primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives = [("+", numericBinop (+)),
    ("-", numericBinop (-)),
    ("*", numericBinop (*)),
    ("/", numericBinop div),
    ("=", numBoolBinop (==)),
    ("<", numBoolBinop (<)),
    (">", numBoolBinop (>)),
    ("/=", numBoolBinop (/=)),
    (">=", numBoolBinop (>=)),
    ("<=", numBoolBinop (<=)),
    ("&&", boolBoolBinop (&&)),
    ("||", boolBoolBinop (||)),
    ("string=?", strBoolBinop (==)),
    ("string<?", strBoolBinop (<)),
    ("string>?", strBoolBinop (>)),
    ("string<=?", strBoolBinop (<=)),
    ("string>=?", strBoolBinop (>=)),
    ("mod", numericBinop mod),
    ("quotient", numericBinop quot),
    ("remainder", numericBinop rem)]

numericBinop :: (Integer -> Integer -> Integer) 
             -> [LispVal] 
             -> ThrowsError LispVal
numericBinop op single@[_] = throwError $ NumArgs 2 single
numericBinop op params = mapM unpackNum params >>= return . Number . foldl1 op

numBoolBinop = boolBinop unpackNum
strBoolBinop = boolBinop unpackStr
boolBoolBinop = boolBinop unpackBool

unaryOp :: (LispVal -> LispVal) 
        -> [LispVal] 
        -> ThrowsError LispVal
unaryOp func [arg] = return $ func arg

boolBinop :: (LispVal -> ThrowsError a) 
          -> (a -> a -> Bool) 
          -> [LispVal] 
          -> ThrowsError LispVal
boolBinop unpacker op [x,y] = do
    left <- unpacker x
    right <- unpacker y
    return $ Bool $ left `op` right
boolBinop _ _ args = throwError $ NumArgs 2 args

unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number n) = return n
unpackNum (List [n]) = unpackNum n
unpackNum notNum = throwError $ TypeMismatch "number" notNum

unpackStr :: LispVal -> ThrowsError String
unpackStr (String s) = return s
unpackStr (Number s) = return $ show s
unpackStr (Bool s) = return $ show s
unpackStr notStr = throwError $ TypeMismatch "string" notStr

unpackBool :: LispVal -> ThrowsError Bool
unpackBool (Bool b) = return b
unpackBool notBool = throwError $ TypeMismatch "boolean" notBool

-- Error checking
-- Defining the types of possible errors
data LispError = NumArgs Integer [LispVal]
    | TypeMismatch String LispVal
    | Parser ParseError
    | BadSpecialForm String LispVal
    | NotFunction String String
    | UnboundVar String String
    | Default String

-- Print error
showError :: LispError -> String
showError (TypeMismatch expected found) = "Invalid type: expected " ++ expected
    ++ ", found " ++ show found
showError (Parser parseErr) = "Parse error at " ++ show parseErr
showError (BadSpecialForm message form) = message ++ ": " ++ show form
showError (NotFunction message func) = message ++ ": " ++ show func
showError (UnboundVar message varname) = message ++ ": " ++ varname
showError (NumArgs expected found) = "Expected " ++ show expected
    ++ " args; found values " ++ unwordsList found

instance Show LispError where show = showError

instance Error LispError where
    noMsg = Default "An error has occurred"
    strMsg = Default

type ThrowsError = Either LispError

-- we'll be converting all of our errors to their string representations and returning that as a normal value
-- The result of calling trapError is another Either action which will always have valid (Right) data.
trapError action = catchError action (return . show)

-- We still need to extract that data from the Either monad so it can be passed around to other functions
extractValue :: ThrowsError a -> a
extractValue (Right val) = val

-- The built-in function lookup looks up a key (its first argument) in a list of pairs. However, lookup will fail if no pair in the list contains the matching key. To express this, it returns an instance of the built-in type Maybe.
apply :: String -> [LispVal] -> ThrowsError LispVal
apply func args = maybe (throwError $ NotFunction "Unrecognized primitive function args" func)
    ($ args)
    (lookup func primitives)

-- Evaluator primitives using Pattern Matching
-- By pattern-matching against cons itself instead of a literal list, we're saying "give me the rest of the list" instead of "give me the second element of the list".
-- The notation val@(String _) matches against any LispVal that's a string and then binds val to the whole LispVal, and not just the contents of the String constructor.
eval :: LispVal -> ThrowsError LispVal
eval val@(String _) = return val
eval val@(Number _) = return val
eval val@(Bool _) = return val
eval (List [Atom "quote", val]) = return val
eval (List [Atom "if", pred, conseq, alt]) = 
     do result <- eval pred
        case result of
             Bool False -> eval alt
             otherwise  -> eval conseq
eval (List (Atom func : args)) = mapM eval args >>= apply func
eval badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm

-- We name the parameter input, and pass it, along with the symbol parser we defined above to the Parsec function parse.
-- The second parameter to parse is a name for the input.
-- Parse can return either the parsed value or an error, so we need to handle the error case.
--  Following typical Haskell convention, Parsec returns an Either data type:
--      - The Left constructor to indicate an error.
--      - the Right one for a normal value.
-- We use a case...of construction to match the result of parse against these alternatives.
-- Bind to spaces: attempt to match the first parser, then attempt to match the second with the remaining input, and fail if either fails.
readExpr :: String -> ThrowsError LispVal
readExpr input = case parse parseExpr "lisp" input of
    Left err -> throwError $ Parser err
    Right val -> return val

main :: IO ()
main = do
    args <- getArgs
    evaled <- return $ liftM show $ readExpr (args !! 0) >>= eval
    putStrLn $ extractValue $ trapError evaled
