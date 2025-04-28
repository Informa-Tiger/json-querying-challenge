{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where
import qualified QueryJSON as Q

-- scotty
import qualified Web.Scotty.Trans as S
import Web.Scotty.Internal.Types (ScottyT)
-- wai-extra
import Network.Wai.Middleware.RequestLogger ( logStdoutDev )
-- wai-cors
import Network.Wai.Middleware.Cors (simpleCors)
-- http-types
import Network.HTTP.Types (status400)
-- aeson
import qualified Data.Aeson as A
import Data.Aeson ( (.=) )
-- sqllite-simple
import qualified Database.SQLite.Simple as DB
-- base
import System.Environment (getArgs)
import System.Environment.Blank (getEnvDefault)
import GHC.IO.Handle.FD (withFile)
import GHC.IO.IOMode (IOMode(ReadMode))
import Data.Maybe (fromMaybe)

-- mtl
import Control.Monad.Reader ( ReaderT(runReaderT), MonadReader(ask) )
-- text
import Data.Text.Lazy.Encoding (decodeUtf8)
import Data.Text.Lazy(toStrict, pack)
--bytestring
import Data.ByteString (hGetContents, fromStrict)
--time
import Data.Time.Clock (UTCTime)


type ReaderIO = ReaderT (DB.Connection, Q.Table) IO

newtype HistoricQuery = Hist (Int, UTCTime, String, Bool, String) 
instance A.ToJSON HistoricQuery where
  toJSON (Hist (idNo, timestamp, query, success, result)) = A.object ["id" .= idNo, "timestamp" .= timestamp, "query" .= query, "success" .= success, "result" .= result]
instance DB.FromRow HistoricQuery where
  fromRow = Hist <$> DB.fromRow
createHistoricQueriesTable :: DB.Query 
createHistoricQueriesTable = "create table if not exists Queries (id INTEGER PRIMARY KEY, timestamp DATETIME DEFAULT CURRENT_TIMESTAMP, query TEXT NON NULL, success BOOLEAN NON NULL, result TEXT NON NULL)"


main :: IO ()
main = do
  args <- getArgs
  json <- withFile (head args) ReadMode hGetContents
  let dbPath = case tail args of {[] -> "./db.db"; path:_ -> path}
  let table :: Q.Table = fromMaybe (error "Could not decode JSON table.")  (A.decode (fromStrict json))
  port :: Int <- read <$> getEnvDefault "PORT" "8000"
  DB.withConnection dbPath $ \conn -> do
    DB.execute_ conn createHistoricQueriesTable
    S.scottyT port (\a -> runReaderT a (conn, table)) app

app :: ScottyT ReaderIO ()
app = do
    S.middleware logStdoutDev
    S.middleware simpleCors
    S.get "/" $ do
        S.html "<b>Hello</b>"
    S.post "/query" $ do
      (conn, table) <- ask
      queryStr <- toStrict . decodeUtf8 <$> S.body
      let saveInDB success result = S.liftIO $ DB.execute conn "insert into Queries (query, success, result) values (?, ?, ?)" (queryStr, success, result)
      case Q.parseQuery queryStr of
        Left err  -> do
          saveInDB False ("expected " ++ err)
          S.status status400
          S.text (pack ("expected " ++ err))
        Right query ->
          case Q.queryTable table query of
            Left err -> do
              saveInDB False err
              S.status status400 
              S.text (pack err)
            Right rows -> do
              let result = A.object["columns" .= Q.getColumnNames table query, "rows" .= rows]
              saveInDB True (decodeUtf8 (A.encode result))
              S.json result

    S.get "/history" $ do
      (conn, _) <- ask
      history :: [HistoricQuery] <- S.liftIO $ DB.query_ conn "select * from Queries order by id desc" 
      S.json history
