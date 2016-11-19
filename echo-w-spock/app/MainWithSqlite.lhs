Connect to Database
--------------------------------------------------------------------------------
Create a sample database with sqlite

In order to do that we will use

Use the file `example_login.sql`

```sqlite3
-- This file is used to create the example database


```
Once you have created the database in order to use with spock it is necesssary to write a connection.

> {-# LANGUAGE OverloadedStrings #-}
> module Main where
>
> import Crypto.BCrypt (validatePassword)
> import Control.Monad (guard)
> import Control.Monad.IO.Class (MonadIO)
> import Data.Aeson (object, (.=))
> import Data.ByteString (ByteString)
> import Data.List (lookup)
> import Data.Maybe (listToMaybe)
> import Data.Text (Text)
> import qualified Data.Text as Text
> import qualified Data.Text.Lazy as TL
> import qualified Data.Text.Encoding as Text
> import qualified Database.SQLite.Simple as SQLite
> import Network.Wai.Middleware.RequestLogger (logStdoutDev)
> import Text.Mustache
> import Web.Spock
> import Web.Spock.Config
>
> findCredential :: MonadIO m => ActionCtxT ctx m (Maybe (Text, Text))
> findCredential = do
>   username <- param "username"
>   password <- param "password"
>   pure $ (,) <$> username <*> password
>
> validateCredential :: (Text, Text) -> ActionCtxT ctx (WebStateM SQLite.Connection sess st) (Maybe Int)
> validateCredential (username, password) = do
>   user <- runQuery $ \conn -> do
>     results <- SQLite.query conn "SELECT id, password FROM user WHERE username = ?" (SQLite.Only (Text.encodeUtf8 username))
>     pure $ (listToMaybe results :: Maybe (Int, ByteString))
>   pure $ do
>     (userId, passhash) <- user
>     guard $ validatePassword passhash (Text.encodeUtf8 password)
>     pure userId
>
> main :: IO ()
> main = do
>   -- データベースの設定
>   let sqliteConnect = SQLite.open "login_sample.sqlite3"
>       dbConn = ConnBuilder sqliteConnect SQLite.close (PoolCfg 1 1 60)
>
>   -- stache の設定
>   template <- compileMustacheDir "index" "views/"
>   let render pname = TL.toStrict . renderMustache (template {templateActual = pname})
>
>   -- Spock の設定と実行
>   spockCfg <- defaultSpockCfg (Nothing :: Maybe Int) (PCConn dbConn) ()
>   runSpock 8080 $ fmap (logStdoutDev.) $ spock spockCfg $ do
>
>     -- GET /
>     get root $ do
>       maybeUserId <- readSession
>       case maybeUserId of
>         Nothing -> redirect "/login"
>         Just userId -> html (render "index" (object ["text" .= (Text.pack (show userId))]))
>
>     -- GET /logout
>     get "logout" $ writeSession Nothing >> redirect "/login"
>
>     -- GET /login
>     get "login" $ html (render "login" (object []))
>
>     -- POST /login
>     post "login" $ do
>       credential <- findCredential
>       case credential of
>         Nothing -> redirect "/login"
>         Just (username, password) -> do
>           maybeUserId <- validateCredential (username, password)
>           case maybeUserId of
>             Nothing -> redirect "/login"
>             Just userId -> writeSession (Just userId) >> redirect "/"
>
>
>
