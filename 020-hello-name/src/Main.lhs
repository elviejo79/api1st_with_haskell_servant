Hello {name}
======================================

Now we are going to improve into our example.
Making the api respond "Hello World!" to the request `/hello/world/`
and respond "Hi, {name}, Nice to see you." to the request `hello/{name}`

The first part of your `/hello-world/src/Main.hs` file stays the same

>{-# LANGUAGE DataKinds       #-}
>{-# LANGUAGE TemplateHaskell #-}
>{-# LANGUAGE TypeOperators   #-}
> module Main where
> import Data.Aeson
> import Data.Aeson.TH
> import Network.Wai
> import Network.Wai.Handler.Warp
> import Servant
>


Designing the API
---------------------------------

* When requested at the endpoint `/hello/{name}`
* Respond with an encoded JSON
* The function that returns the value must return a String

What we need is to `Capture` the `name` parameter as a `String` and then pass it to the function.

> type HwAPI = "hello" :> Capture "name" String :> Get '[JSON] String


Implementing the functions
----------------------------------
Now we need to change how our function behaves
It will now recevie `name` (that is a String) and return the salute that is another String.
If the `name` variable is "world" then it will return a message if it is something else it will say something else.

> sayHello :: String -> String
> sayHello "world" = "Hello World!"
> sayHello name    = "Hi, " ++ name ++ ", nice seeing you"


Associate the API with the implementation function
------------------------------------

> hwServer :: Server HwAPI
> hwServer = hSayHello
>   where hSayHello name = return $ sayHello name
>

And that's it... all the other code in `Main.hs` remains the same.


Boilerplate to serve the API
------------------------------------

>
> -- Create the proxy
> hwProxy :: Proxy HwAPI
> hwProxy = Proxy
>
> -- Create a WAI application.
> hwApp :: Application
> hwApp = serve hwProxy hwServer
>
> -- Associate the App with the webserver
> main :: IO()
> main = run 8080 hwApp


Execute the web server
-------------------------------------

```shell
$ stack build && stack exec hello-world
```

and Test the API
-------------------------------------
In your browser go to:
http://localhost:8080/hello/michelle/
and then to
http://localhost:8080/hello/world/

And see the different answers.
