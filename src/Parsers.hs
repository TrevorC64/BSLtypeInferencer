module Parsers
where
        
import Text.ParserCombinators.Parsec hiding (spaces, Line)

data Expr = -- Primatives --  
              Var String
            | Number Integer
            | String String
            | Bool Bool
            -- Expressions --
            | UniOp String Expr
            | BinOp String Expr Expr
            | Cond [(Expr, Expr)]
            | Else
            | Cons Expr Expr
            | Empty
            | Let [(String, Expr)] Expr
            -- Lambda --
            | Lamb [String] Expr
            | App Expr Expr
        deriving (Show)

data Definition = Func String [String] Expr        -- (define (func a) a)
              | Const String Expr                -- (define x 0)
        deriving (Show)

data Comment = Comm String
             | DataDef String [String]          -- ; A Name is a String
             | Signat String [String] String    -- ; fib : Number -> Number
        deriving (Show)

data SExpr = Exp Expr 
            | Def Definition
            | Com Comment
            -- Error --
            | Err String
        deriving (Show)

prettyString :: String -> String
prettyString str = filter (not . (`elem` "\"\'\\")) (show str)

spaces :: Parser ()
spaces = skipMany1 space

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

determiner :: Parser String
determiner = try (string "An")
         <|> try (string "an") 
         <|> try (string "A") 
         <|> try (string "a")
        
parseName :: Parser String
parseName = do
    first <- letter <|> symbol
    rest <- many (letter <|> digit <|> symbol)
    return $ first:rest

parseNumber :: Parser SExpr
parseNumber = do
    x <- many1 digit
    return $ Exp $ (Number . read) x

parseString :: Parser SExpr
parseString = do
    char '"'
    x <- many1 $ noneOf "\""
    char '"' 
    return $ Exp $ String x

parseVar :: Parser SExpr
parseVar = do 
    var <- parseName
    return $ case var of 
                "#t"   -> Exp $ Bool True
                "#f"   -> Exp $ Bool False
                "else" -> Exp $ Else
                _      -> Exp $ Var var

---------------------------------------------------------------------
-- ### COMMENT PARSERS ###
---------------------------------------------------------------------
endOfComment :: Parser String
endOfComment = (try (string "\r\n") <|> try (string "\r") <|> try (string "\n"))

parseComment :: Parser SExpr
parseComment = do
    char ';'
    comment <- manyTill anyChar endOfComment
    return $ Com $ Comm comment

parseDataDefBasic :: Parser SExpr
parseDataDefBasic = do
    char ';'
    skipMany space
    determiner
    spaces
    d <- many1 letter
    skipMany space 
    string "is"
    skipMany space
    optional determiner
    skipMany space
    ty <- manyTill anyChar endOfComment
    return $ Com $ DataDef d [ty]

parseSignat :: Parser SExpr
parseSignat = do
    char ';'
    skipMany space
    func <- many (letter <|> digit <|> symbol)
    skipMany space
    char ':'
    skipMany space
    atys <- manyTill argumentParser $ string "->"
    skipMany space
    rty <- manyTill (letter <|> digit <|> symbol) endOfComment
    return $ Com $ Signat func atys rty
    
argumentParser :: Parser String
argumentParser = do
    w <- many (letter <|> digit <|> symbol)
    skipMany space
    return w

parseComments :: Parser SExpr
parseComments =  try parseDataDefBasic
             <|> try parseSignat
             <|> try parseComment
 
---------------------------------------------------------------------
-- ### DEF PARSERS ###
---------------------------------------------------------------------
parseConst :: Parser SExpr
parseConst = do
    char '('
    string "define"
    spaces
    name <- many1 (letter <|> digit <|> symbol)
    spaces
    (Exp expr) <- parseExp
    char ')'
    return $ Def $ Const (prettyString name) expr

parseFunc :: Parser SExpr
parseFunc = do
    char '('
    string "define"
    spaces
    char '('
    name <- many1 (letter <|> digit <|> symbol)
    spaces 
    args <- manyTill parseArg $ char ')'
    spaces
    (Exp expr) <- parseExp
    char ')'
    return $ Def $ Func (prettyString name) (map prettyString args) expr    

parseArg:: Parser String
parseArg = do
    v <- many1 (letter <|> digit <|> symbol)
    optional spaces
    return v

parseDef :: Parser SExpr
parseDef =  try parseFunc
        <|> try parseConst

---------------------------------------------------------------------
-- ### EXPR PARSERS ###
---------------------------------------------------------------------

parseUniOp :: Parser SExpr
parseUniOp = do
    char '('
    op  <- manyTill anyChar spaces
    (Exp arg) <- parseExp
    char ')'
    return $ Exp $ UniOp (prettyString op) arg
    

parseBinOp :: Parser SExpr
parseBinOp = do
    char '('
    op <- manyTill anyChar spaces
    (Exp x) <-  parseExp
    spaces
    (Exp y) <-  parseExp
    char ')'
    return $ Exp $ BinOp (prettyString op) x y

parseCond :: Parser SExpr
parseCond = do
    string "(cond"
    spaces
    cases <- manyTill parseCondCase $ char ')'
    return $ Exp $ Cond cases

parseCondCase :: Parser (Expr, Expr)
parseCondCase = do
    char '['
    (Exp test) <- parseExp
    spaces
    (Exp res) <- parseExp
    char ']'
    skipMany space
    return $ (test, res)

parseCons :: Parser SExpr
parseCons = do
    string "(cons"
    skipMany space
    (Exp a) <- parseExp
    skipMany space
    (Exp d) <- parseExp 
    char ')'
    return $ Exp $ Cons a d

parseEmpty :: Parser SExpr
parseEmpty = do
    string "empty"
    return $ Exp $ Empty

parseLet :: Parser SExpr
parseLet = do
    string "(let"
    skipMany space
    char '('
    bindings <- manyTill parseLetBinding $ char ')'
    skipMany space
    (Exp body) <- parseExp
    char ')'
    return $ Exp $ Let bindings body

parseLetBinding :: Parser (String, Expr)
parseLetBinding = do
    char '['
    var <- parseName
    spaces
    (Exp val) <- parseExp
    skipMany space
    char ']'
    skipMany space
    return $ (var, val)

parseLambda :: Parser SExpr
parseLambda = do
    string "(lambda"
    spaces
    char '('
    vars <- manyTill parseName $ char ')'
    spaces
    (Exp body) <- parseExp
    char ')'
    return $ Exp $ Lamb vars body

parseApp :: Parser SExpr
parseApp = do
    char '('
    (Exp rator) <- parseLambda
    spaces 
    (Exp rand) <- parseExp
    char ')'
    return $ Exp $ App rator rand

parseExp :: Parser SExpr
parseExp = do try parseCons
          <|> try parseCond
          <|> try parseLet
          <|> try parseLambda
          <|> try parseApp
          <|> try parseBinOp
          <|> try parseUniOp
          <|> try parseEmpty
          <|> try parseVar
          <|> try parseNumber
          <|> try parseString 

--------------------------------------------------------
-- ### INFERENCER ###
--------------------------------------------------------
infExp :: Expr -> String
infExp (Var _)           = "Symbol"
infExp (Number _)        = "Number"
infExp (String _)        = "String"
infExp (Bool _)          = "Boolean"
infExp (UniOp func _)    | elem func ["abs", "add1", "sub1", "string->number", "string-length", "sin", "cos"] = "Number"
                         | '?' == head (reverse func) = "Boolean"        -- making the assumption that all uninary functions WT {* -> Boolean} end with ?
infExp (BinOp func _ _)  | elem func ["+", "-", "*", "/", "%", "expt"] = "Number"
                         | elem func ["and", "or", "<", "<=", ">", ">=", "equal?", "string=?", "string>?", "string<?"] = "Boolean"
infExp (Cond (c:cs))     = infExp $ snd c
infExp (Cons a d)        = "ListOf" ++ infExp a -- does not inforce all elements are same type
infExp (Lamb x b)        = "X -> " ++ infExp b 
infExp Empty             = "ListOfa"

infSExpr :: SExpr -> String
infSExpr (Exp e) = infExp e
infSExpr (Def _) = "Definition"
infSExpr (Com _) = "Comment"
infSExpr (Err _) = "Error"
 
--------------------------------------------------------
-- ### RUNNING ###
--------------------------------------------------------

interp :: String -> SExpr
interp s_exp = do
    case parse parseSExpr "BSL" s_exp of
        Left err -> Err $ show err
        Right e -> e

infer :: String -> String
infer s_exp = do
    case parse parseSExpr "BSL" s_exp of
        Left err -> show err
        Right e -> infSExpr e

parseSExpr :: Parser SExpr
parseSExpr =  do 
    exp <-  try parseDef
        <|> try parseComments
       <|> try parseExp
    optional $ string "\r\n"
    optional $ char '\n'
    optional $ string "\r"
    return exp

parseFile :: Parser [SExpr]
parseFile = do
    manyTill parseSExpr eof

mainInterp :: Int -> IO ()
mainInterp  n  = do{ result <- parseFromFile parseFile $ "tests/test"++(show n)++".txt"
             ; case result of
                 Left err  -> print err
                 Right xs  -> print xs
             }

mainInfer :: Int -> IO ()
mainInfer  n  = do{ result <- parseFromFile parseFile $ "tests/test"++(show n)++".txt"
             ; case result of
                 Left err  -> print err
                 Right xs  -> print $ infSExpr $ last xs
             }
