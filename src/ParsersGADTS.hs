{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-} 
{-# LANGUAGE DeriveDataTypeable #-}
module ParsersGADTS
where
        
import Text.ParserCombinators.Parsec hiding (spaces, Line)

data Definition where
    Func    :: String -> [String] -> Expr a -> Definition
    Const   :: String -> Expr a -> Definition

data Comment where
    Note    :: String -> Comment
    DataDef :: String -> [String] -> Comment
    Signat  :: String -> [String] -> String -> Comment

data Expr a where
    -- Primatives --  
    Var     :: String -> Expr String
    Number  :: Int -> Expr Int
    String  :: String -> Expr String
    Bool    :: Bool -> Expr Bool
    -- Expressions --
    UniOp   :: String -> Expr a -> Expr b
    BinOp   :: String -> Expr a -> Expr b -> Expr c
    --
    Cond    :: [(Expr Bool, Expr a)] -> Expr a
    Else    :: Expr Bool
    Cons    :: Expr a -> Expr [a] -> Expr [a]
    Empty   :: Expr [a]
    Let     :: [(String, Expr a)] -> Expr b -> Expr b
    -- Lambda --
    Lamb    :: [String] -> Expr a -> Expr a
    App     :: Expr a -> Expr b -> Expr a

data SExpr where
    Exp  :: (Expr a) -> SExpr
    Def  :: Definition -> SExpr
    Com  :: Comment    -> SExpr
    -- Error --
    Err :: String     -> SExpr


instance Show Definition where
    show (Func n args body)  = "(define (" ++ (show n) ++ " " ++ (foldr (\s a -> a ++ (show s) ++ " ") "" args) ++ ")\n\t" ++ (show body) ++ ")"
    show (Const n e)         = "(define " ++ (show n) ++ " " ++ (show e) ++ ")"
    
instance Show Comment where 
    show (Note c)            = "(Note " ++ (show c) ++ ")"
    show (DataDef s tys)     = case (length tys) of
                                1 -> "(DataDef " ++ (show s) ++ " " ++ (show $ head tys) ++ ")"
                                _ -> "" -- not implemented
    show (Signat n tys rty)  = "(Signat " ++ (show n) ++ " : " ++ (foldr (\s a -> a ++ (show s) ++ " ") "" tys) ++ " -> " ++ (show rty) ++ ")"
    
instance Show (Expr a) where
    -- Primatives --  
    show (Var a)             = "(Var " ++ (show a) ++ ")"
    show (Number n)          = "(Number " ++ (show n) ++ ")"
    show (String s)          = "(String " ++ (show s) ++ ")"
    show (Bool b)            = "(Bool " ++ (show b) ++ ")"
    -- Expressions --
    show (UniOp f a)         = "(UniOp \"" ++ (show f) ++ "\" " ++ (show a) ++ ")" 
    show (BinOp f a b)       = "(BinOp \"" ++ (show f) ++ "\" " ++ (show a) ++ " " ++ (show b) ++ ")" 
    show (Cond cases)        = "(Cond   " ++ (foldr (\(t, r) a -> a ++ "[" ++ (show t) ++ " " ++ (show r) ++ "]\n\t") "" cases) ++ ")" 
    show Else                = "Else"
    show (Cons a d)          = "(Cons " ++ (show a) ++ " " ++(show d) ++ ")"
    show Empty               = "Empty"
    show (Let bindings body) = "(Let   " ++ (foldr (\(v, a) acc -> acc ++ "[" ++ (show v) ++ " " ++ (show a) ++ "]\n\t") "" bindings) ++ (show body) ++")"
    -- Lambda --
    show (Lamb vars body)    = "(Lambda (" ++ (foldr (\s a -> a ++ (show s) ++ " ") "" vars) ++ ")\n  " ++ (show body) ++ ")"
    show (App rator rand)    = "(" ++(show rator) ++ (show rand) ++ ")"

instance Show SExpr where
    show (Exp e)             = show e
    show (Def d)             = show d
    show (Com c)             = show c
    -- Error --
    show (Err e)             = "(Error " ++ (show e) ++ ")"


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
    var <- try parseName
    return $ Exp $ Var var

parseBool :: Parser SExpr
parseBool = do 
    b <- (try (string "#t")) <|> (try (string "#f")) <|> (try (string "else"))
    return $ case b of 
                "#t" -> Exp $ (Bool True)
                "#f" -> Exp $ (Bool False)
                "else" -> Exp $ (Bool True)


parsePrim :: Parser SExpr
parsePrim =  do try parseNumber
            <|> try parseVar
            <|> try parseNumber
            <|> try parseString



---------------------------------------------------------------------
-- ### COMMENT PARSERS ###
---------------------------------------------------------------------
endOfComment :: Parser String
endOfComment = (try (string "\r\n") <|> try (string "\r") <|> try (string "\n"))

parseNote :: Parser SExpr
parseNote = do
    char ';'
    comment <- manyTill anyChar endOfComment
    return $ Com $ Note comment

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


parseComment :: Parser SExpr
parseComment =  try parseDataDefBasic
            <|> try parseSignat
            <|> try parseNote
            

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
    
{-
parseCond :: Parser SExpr
parseCond = do
    string "(cond"
    spaces
    cases <- manyTill parseCondCase $ char ')'
    return $ Exp $ Cond $ (map (\((Exp s1), (Exp s2)) -> (s1, s2)) cases)

parseCondCase :: Parser (SExpr, SExpr)
parseCondCase = do
    char '['
    test <- parseExp
    spaces
    res  <- parseExp
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
-}
parseEmpty :: Parser SExpr
parseEmpty = do
    string "empty"
    return $ Exp $ Empty
{-
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

parseLetBinding :: Parser (String, (Expr a))
parseLetBinding = do
    char '['
    var <- parseName
    spaces
    (Exp val) <- parseExp
    skipMany space
    char ']'
    skipMany space
    return $ (var, val)
-}
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
parseExp = do try parseLambda
       --   <|> try parseCond
       --   <|> try parseLet
       --   <|> try parseCons
          <|> try parseApp
          <|> try parseBinOp
          <|> try parseUniOp
          <|> try parseEmpty
          <|> try parseVar
          <|> try parseNumber
          <|> try parseString 


{-
--------------------------------------------------------
-- ### INFERENCER ###
--------------------------------------------------------
infExp :: Expr -> String
infExp (Var _)          = "Symbol"
infExp (Number _)        = "Number"
infExp (String _)        = "String"
infExp (Bool _)          = "Boolean"
infExp (UniOp func _)    | elem func ["abs", "add1", "sub1", "string->number"] = "Number"
                         | '?' == head (reverse func) = "Boolean"        -- making the assumption that all uninary functions WT {* -> Boolean} end with ?
infExp (BinOp func _ _)  | elem func ["+", "-", "*", "/", "%", "expt"] = "Number"
                         | elem func ["and", "or", "<", "<=", ">", ">=", "equal?"] = "Boolean"
infExp (Cond (c:cs))     = infExp $ snd c
infExp (Cons a d)        = "ListOf" ++ infExp a -- does not inforce all elements are same type
infExp Empty             = "ListOfa"
 
--------------------------------------------------------
-- ### RUNNING ###
--------------------------------------------------------

interp :: String -> Expr
interp s_exp = do
    case parse parseExpr "BSL" s_exp of
        Left err -> Err $ show err
        Right e -> e

infer :: String -> String
infer s_exp = do
    case parse parseExpr "BSL" s_exp of
        Left err -> show err
        Right e -> infExp e


parseExpr :: Parser Expr
parseExpr =  do 
    exp <-  try parseDef
        <|> try parseComment
        <|> try parseExp
    optional $ string "\r\n"
    optional $ char '\n'
    optional $ string "\r"
    return exp

parseFile :: Parser [Expr]
parseFile = do
    manyTill parseExpr eof

main :: Int -> IO ()
main  n  = do{ result <- parseFromFile parseFile $ "tests/test"++(show n)++".txt"
             ; case result of
                 Left err  -> print err
                 Right xs  -> print xs
             }
-}