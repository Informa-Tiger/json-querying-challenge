{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Attoparsec.Text as P
import Data.Attoparsec.Text (Parser, (<?>))
import Control.Applicative 
import Data.Char (isAlphaNum, isAlpha)
import Data.Text as T
import GHC.Utils.Misc
import Data.Functor
import Data.Scientific
import Control.Monad()
import Data.Aeson(eitherDecodeStrict, Value)
import qualified Data.Aeson.Types as A
import Data.Map(Map, (!))
import Data.Primitive.Array
import Data.Foldable (Foldable(toList))
import Foreign.Marshal.Utils(fromBool)
import Data.ByteString.UTF8 as BSU
import System.IO(hFlush, stdout)


data Query = Query {
      columns :: [Text]
    , whereClause  :: Expression
    , limit :: Maybe Int
    } deriving Show


sqlParser :: Parser Query
sqlParser = do
    keywordParser "select"
    columns <- P.sepBy1 columnNameParser (P.skipSpace *> P.char ',')
    keywordParser "from"
    keywordParser "table"
    hasWhereClause <- P.option False (keywordParser "where" $> True)
    whereClause <- if hasWhereClause then expressionParser else pure (Num 1)
    hasLimit <- P.option False (keywordParser "limit" $> True)
    limit <- if hasLimit then Just <$> (P.skipSpace *> P.decimal) else pure Nothing
    P.skipSpace
    _ <- P.char ';'
    P.skipSpace
    P.endOfInput
    return Query{columns, whereClause, limit}

delimitedParser :: Char -> Parser Text
delimitedParser delimeter = do 
        P.skipSpace
        _ <- P.char delimeter
        chars <- P.many' (P.satisfy (/= delimeter) <|> (P.char delimeter *> P.char delimeter))
        _ <- P.char delimeter
        return (T.pack chars)

columnNameParser :: Parser Text
columnNameParser =
    do
        P.skipSpace
        x <- P.satisfy ((== '_' )<||> isAlpha)
        rest <- P.takeWhile ((== '_') <||> isAlphaNum)
        return (cons x rest)
    <|> delimitedParser '"' 

keywordParser :: Text -> Parser ()
keywordParser keyword = do
    P.skipSpace
    _ <- P.asciiCI keyword
    next <- P.peekChar
    case next of Just x | ((== '_') <||> isAlphaNum) x -> fail "expected keyword"
                 _ -> return ()

data BinaryOperator = Eq | Neq | Lt | Gt | And | Or deriving Show
data Expression = Col Text | Str Text | Num Scientific | BinOp BinaryOperator Expression Expression deriving Show
    
-- https://www.sqlite.org/lang_expr.html#operators_and_parse_affecting_attributes
expressionParser :: Parser Expression
expressionParser = do
    binaryOperationParser   (keywordParser "or")    (BinOp Or)
    $ binaryOperationParser (keywordParser "and")   (BinOp And)
    $ binaryOperationParser (P.char '=')            (BinOp Eq)
    $ binaryOperationParser (P.string "!=")         (BinOp Neq)
    $ binaryOperationParser (P.char '>')            (BinOp Gt)
    $ binaryOperationParser (P.char '<')            (BinOp Lt)
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

binaryOperationParser :: Parser a -> (Expression -> Expression -> Expression) -> Parser Expression -> Parser Expression
binaryOperationParser operatorParser operator higherPrecedence = do
    exp1 <- higherPrecedence
    parseRest exp1 <|> return exp1
    where
        parseRest ::  Expression -> Parser Expression
        parseRest exp1 = do
            P.skipSpace
            _ <- operatorParser
            exp2 <- higherPrecedence
            let expr = operator exp1 exp2
            parseRest expr <|> return expr

queryTable :: Array(Map String Value) -> Query -> [[Value]]
queryTable table query = 
    [[row ! unpack col | col <- columns query] | row <- toList table, asBool (evalExpression row (whereClause query))]

asBool :: Value -> Bool
asBool (A.Bool val) = val
asBool (A.Number val) = val /= 0
asBool _ = False

evalExpression :: Map String Value -> Expression -> Value
evalExpression row (Col col) = case row ! unpack col of {A.Bool val -> A.Number (fromBool val); val -> val}
evalExpression _   (Str str) = A.String str
evalExpression _   (Num val) = A.Number val
evalExpression row (BinOp op exp1 exp2) =
    let val1 = evalExpression row exp1 in
    let val2 = evalExpression row exp2 in
    if ((val1 == A.Null) || (val2 == A.Null)) then A.Null
    else A.Number $ fromBool $ case op of
        Eq -> val1 == val2
        Neq -> val1 /= val2
        Gt -> val1 > val2
        Lt -> val1 < val2
        And -> (asBool val1) && (asBool val2)
        Or -> (asBool val1) || (asBool val2)

main :: IO ()
main = do
    putStr "Table JSON: "
    hFlush stdout
    tableJSON <- BSU.fromString <$> getLine -- i.e. [{"a":1, "b":"foo"}, {"a": 2, "b":"bar"}]
    table :: Array (Map String Value) <- case eitherDecodeStrict tableJSON of 
            Left err -> fail err
            Right res -> pure res
    putStr "SQL Query: "
    hFlush stdout
    queryStr <- pack <$> getLine -- i.e. "Select b,a from table where a > 1;"
    query <- case P.parseOnly sqlParser queryStr of
            Left err -> fail err
            Right res -> pure res
    print query
    let result = queryTable table query
    print result
