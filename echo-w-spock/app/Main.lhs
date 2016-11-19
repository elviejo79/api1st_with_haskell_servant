Introduction to web Developmen with Spock
==========================================================
Implementation of the tutorial on this http://qiita.com/lotz/items/e2c6765e65d7eb692ee2

Spock is a lightweight Haskell web framework inspired by Ruby's Sinatra.
It provides a full toolbox including everything to get a quick start into web
programming using Haskell.

First of all we need to include Spock and Config.

> {-# LANGUAGE OverloadedStrings #-}
> module Main where
>
> import Web.Spock
> import Web.Spock.Config
> import qualified Data.Text as T
>
> import Network.HTTP.Types
> import Control.Monad.IO.Class (MonadIO)
> import Data.ByteString (ByteString)
> import Data.List (lookup)
> import Data.Maybe (listToMaybe)
> import Data.Text (Text)
> import Data.Text.Encoding (encodeUtf8)
> import qualified Data.Text as Text
> import Crypto.BCrypt (validatePassword)





As we know main is the only function that actually gets exucuted
`IO` means that is an Input / Output operation with `()` that doesn't return anything

> main :: IO ()
> main = do

With the deault Spokc Configuration and `()` no session management and no database.

>        spockCfg <- defaultSpockCfg () PCNoDatabase ()

Given that confiuration execute the `app` in the port 8080

>        runSpock 8080 (spock spockCfg app)
>

Now the actual spock `app`  The type means that it is in the SpockM (monad)

> app :: SpockM () () () ()
> app =

When calling `/` return "Hello World"

>     do get root $
>            text "Hello World!"

When calling `/hello/{name}`

>        get ("hello" <//> var) $ \name ->
>               text ( T.concat ["Hello ",name])
>        postLogin2
>        postLogin3

Form data
----------------------------------------------------------

>        post "login" $ do
>          maybeUsername <- param "username"
>          case maybeUsername of
>            Nothing       -> setStatus status400 >> text "Missing parameter"
>            Just username -> text ( T.concat ["Username: ",username])
>

You might test it from the command line

```shell
$ curl -XPOST -F'username=lots' http://localhost:8080/login
Username: lotz
```

First Login API
===========================================================
Preparing the set of username and password.


>
> userList ::[(T.Text, T.Text)]
> userList = [("lotz","password1")
>            ,("alice","password2")
>            ,("bob","password3")
>            ]
>
> findCredential :: MonadIO m => ActionCtxT ctx m (Maybe (Text, Text))
> findCredential = do
>   username <- param "username"
>   password <- param "password"
>   pure $ (,) <$> username <*> password -- <$> == fmap <*> == applicative
>

Creating post "login"

> postLogin2 = post "login2" $ do
>   credential <- findCredential
>   case credential of
>     Nothing -> setStatus status404 >> text "Missing parameter."
>     Just (username, password) ->
>       if lookup username userList == Just password
>          then text "Loging succeed."
>          else setStatus status400 >> text "Wrong parameter."

Now we can test the Login
from the commond line

```json
$ curl -XPOST -F'username=lotz' -F 'password=password1' http://localhost:8080/login2
```

Of course, we nee do hash passwords
--------------------------------------------------------------------------------
When encrypting the answer is to use bycrypt

Hashing password in `userList`.

> userList2 :: [(Text, ByteString)]
> userList2 =
>  [ ("lotz", "$2y$04$UMnRYB26AvreBv0v4efdauIIr3qTM0opEKln26tSy6XXmKV4hS56S")
>  , ("alice", "$2y$04$kRpVhhxbgerHneJJ4HqmNe8MIB7WbPJPXXI3Zy0hFhWpiaJIz6t3m")
>  , ("bob", "$2y$04$qbhEesNseMuIpJfFzN7F7uCN6Y5CB0vmMs7eq708CAAx8wnzxvGAm")
>  ]

Create a validation function.

> validateCredential :: [(Text, ByteString)] -> (Text, Text) -> Bool
> validateCredential userList (username, password) =
>   case lookup username userList of
>     Nothing -> False
>     Just passhash -> let password' = encodeUtf8 password
>      in  validatePassword passhash password'

> postLogin3 = post "login3" $ do
>   credential <- findCredential
>   case credential of
>     Nothing -> setStatus status404 >> text "Missing parameter."
>     Just (username, password) ->
>       if validateCredential userList2 (username,password)
>          then text "Loging3 succeed with bcrypt"
>          else setStatus status400 >> text "Wrong password or username."


Now you can test that the login using bcrypt is working
```json
$ curl -XPOST -F'username=lotz' -F 'password=password1' http://localhost:8080/login3
Loging3 succeed with bcrypt
```

Connect to Database
--------------------------------------------------------------------------------
Create a sample database with sqlite

In order to do that we will use

Use the file `example_login.sql`

```sqlite3
-- This file is used to create the example database



```
