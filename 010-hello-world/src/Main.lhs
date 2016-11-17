Hello World
==================================


Our Objective is to create an API that when you visit `/hello/world` it returns a JSON or a Plain text

Using `stack` create the `new hello-world` project using the `simple` template.

```shell
$ stack new hello-world simple
```


now go to `/hello-world/src/Main.hs` and let's start coding


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

* When requested at the endpoint `/hello/world`
* Respond with an encoded JSON
* The function that returns the value must return a String

The type of the API is the *contract*
As long as the customer makes a request in `/hello/world` it will get back a Json encoded String

> type HwAPI = "hello" :> "world" :> Get '[JSON] String

Now define the "function" that will actually respond to the request.

> sayHello :: String
> sayHello = "Hello World!"

Let's now associate the function with the API.
the type `Server XXXX` comes from Servant and it must be specified to the API

> hwServer :: Server HwAPI
> hwServer = return sayHello

Now we need to add a little bit of boilerplate to guide the type inference

> hwProxy :: Proxy HwAPI
> hwProxy = Proxy

With the proxy we create a WAI applicotian.
WAI is used by all haskell web servers.
Think of hwApp as an executable tha get's connected to the web server.
Kind like a cgi perl script

> hwApp :: Application
> hwApp = serve hwProxy hwServer

Now let's associate the App with the webserver

> main :: IO()
> main = run 8080 hwApp

finally execute the web server

```shell
$ stack build && stack exec hello-world
```

Now in your browser go to: http://localhost:8080/helo/world/

And see the JSON encoded response.
