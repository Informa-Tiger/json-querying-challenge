{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE LambdaCase #-}


module QueryJSON where

import qualified Data.Attoparsec.Text as P
import Data.Attoparsec.Text (Parser, (<?>))

import Data.Char (isAlphaNum, isAlpha)
import Foreign.Marshal.Utils(fromBool)
import Data.Maybe(mapMaybe)
import Control.Applicative ( Alternative((<|>)) ) 
import Control.Monad ( foldM, MonadPlus(..) )
import Data.Functor ( ($>) )
import Data.Function ( (&) )

import qualified Data.Text as T
import Data.Text(Text)
import GHC.Utils.Misc ( (<||>) )
import Data.Scientific ( Scientific )
import qualified Data.Aeson.Types as A
import qualified Data.Map as M


data Query = Query {
      columns :: [Selectable]
    , whereClause  :: Expression
    , limit :: Maybe Int
    } deriving Show

type Table = [M.Map String A.Value]

data Selectable = Asterisk | Expr (Text, Expression) deriving Show

sqlParser :: Parser Query
sqlParser = do
    keywordParser "select"
    columns <- P.sepBy1 ((P.skipSpace *> P.char '*' $> Asterisk) <|> (Expr <$> P.match expressionParser)) (P.skipSpace *> P.char ',') <?> "columns"
    keywordParser "from"
    keywordParser "table"
    hasWhereClause <- P.option False (keywordParser "where" $> True)
    whereClause <- if hasWhereClause then expressionParser  <?> "where clause" else pure (Num 1)
    hasLimit <- P.option False (keywordParser "limit" $> True)
    limit <- if hasLimit then Just <$> (P.skipSpace *> P.decimal) <?> "limit" else pure Nothing
    P.skipSpace
    _ <- P.char ';' <?> "final semicolon"
    P.skipSpace
    P.endOfInput 
    return Query{columns, whereClause, limit}

-- Parses a string delimeted by the given char with double delimeters for escape
delimitedParser :: Char -> Parser Text
delimitedParser delimeter = do 
        P.skipSpace
        _ <- P.char delimeter
        chars <- P.many' (P.satisfy (/= delimeter) <|> (P.char delimeter *> P.char delimeter))
        _ <- P.char delimeter
        return (T.pack chars)

-- currently allows reserved keywords as column names
columnNameParser :: Parser Text
columnNameParser =
    do
        P.skipSpace
        x <- P.satisfy ((== '_' )<||> isAlpha)
        rest <- P.takeWhile ((== '_') <||> isAlphaNum)
        return (T.cons x rest)
    <|> delimitedParser '"' 

-- consumes a given word ingoring case and makes sure there is nothing appended to it
keywordParser :: Text -> Parser ()
keywordParser keyword = do
    P.skipSpace
    _ <- P.asciiCI keyword
    next <- P.peekChar
    case next of
        Just x | ((== '_') <||> isAlphaNum) x -> fail "expected end of identifier"
        _ -> return ()
    <?> "keyword " ++ T.unpack keyword

data BinaryOperator = Eq | Neq | Lt | Gt | And | Or deriving Show
data Expression = Col Text | Str Text | Num Scientific | BinOp BinaryOperator Expression Expression deriving Show
    
-- For order of precedence see https://www.sqlite.org/lang_expr.html#operators_and_parse_affecting_attributes
expressionParser :: Parser Expression
expressionParser = do
    binaryOperationParser   (keywordParser "or" $> BinOp Or)
    $ binaryOperationParser (keywordParser "and" $> BinOp And)
    $ binaryOperationParser ((P.char '=' $> BinOp Eq) <|> (P.string "!=" $>  BinOp Neq))
    $ binaryOperationParser ((P.char '>' $> BinOp Gt) <|> (P.char '<' $> BinOp Lt))
    simpleExpressionParser


simpleExpressionParser :: Parser Expression
simpleExpressionParser = P.skipSpace *> (
    do 
        _ <- P.char '('
        clause <- expressionParser
        P.skipSpace
        _ <- P.char ')'
        return clause
    <|> (Col <$> columnNameParser)
    <|> (Str <$> delimitedParser '\'')
    <|> (Num <$> P.scientific <?> "number")
    )

-- parses a (left-associative, infix) binary operation
binaryOperationParser :: Parser (Expression -> Expression -> Expression) -> Parser Expression -> Parser Expression
binaryOperationParser operatorParser higherPrecedence = do
    exp1 <- higherPrecedence
    parseRest exp1 <|> return exp1
    where
        parseRest ::  Expression -> Parser Expression
        parseRest exp1 = do
            P.skipSpace
            operator <- operatorParser
            exp2 <- higherPrecedence
            let expr = operator exp1 exp2
            parseRest expr <|> return expr

queryTable :: Table -> Query -> Either String [[A.Value]]
queryTable table query = 
    let allColumns = map (Col . T.pack) (M.keys (head table)) in
    let selectedColumns = columns query >>= \case {Asterisk -> allColumns; Expr (_, expr) -> [expr]} in
    table &
    map (\row -> (Right asBool <*> evalExpression row (whereClause query), extractEither [evalExpression row col | col <- selectedColumns])) &
    mapMaybe ( \case 
        (Left err, _) -> Just (Left err)
        (Right False, _) -> Nothing
        (Right True, colsOrErr) -> Just colsOrErr
    ) & maybe id take (limit query) & extractEither
    where
        -- returns either first Left value in list or the list with Right extracted
        extractEither :: (Foldable t, MonadPlus t) => t (Either e a) -> Either e (t a)
        extractEither = foldM (\l -> (Right (mplus l . pure) <*>)) mzero

getColumnNames :: Table -> Query -> [String]
getColumnNames table query =
    case table of 
        [] -> []
        firstRow: _ -> 
            let allColumns = M.keys firstRow in
            columns query >>= \case {Asterisk -> allColumns; Expr (name, _) -> [T.unpack name]}


asBool :: A.Value -> Bool
asBool (A.Bool val) = val
asBool (A.Number val) = val /= 0
asBool (A.String val) = case P.parseOnly (P.scientific <|> pure 0) val of Right numPrefix -> numPrefix /= 0
asBool _ = error "Not implemented."

evalExpression :: M.Map String A.Value -> Expression -> Either String A.Value
evalExpression row (Col col) = case M.lookup (T.unpack col) row of 
    Just (A.Bool val) -> Right (A.Number (fromBool val))
    Just (A.Number val) -> Right (A.Number val) 
    Just (A.String val) -> Right (A.String val)
    Just _ -> Left "Illegal value in JSON Table. Only Bool, Number and String are allowed."
    Nothing -> Left ("no such column: \""++ T.unpack col ++"\"")
evalExpression _   (Str str) = Right (A.String str)
evalExpression _   (Num val) = Right (A.Number val)
evalExpression row (BinOp op exp1 exp2) =
    case (evalExpression row exp1, evalExpression row exp2) of
        (Left err, _ ) -> Left err
        (_, Left err) -> Left err 
        (Right val1, Right val2) ->
            Right $ A.Number $ fromBool $ case op of
                Eq -> val1 == val2
                Neq -> val1 /= val2
                Gt -> val1 > val2 -- Gt, Lt do not work for String Number comparison
                Lt -> val1 < val2
                And -> asBool val1 && asBool val2
                Or -> asBool val1 || asBool val2

parseQuery = P.parseOnly sqlParser
