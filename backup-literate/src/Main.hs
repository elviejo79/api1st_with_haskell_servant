# Hello World
Our Objective is to create an API that when you visit `/hello/world` it returns a JSON or a Plain text

Let's create a new project using stack

```shell
$ stack new hello-world simple
```
Which means create the `hello-world` project using the `simple` template.
now go to `/hello-world/src/Main.hs` and let's start coding



> module Main where
>
> import Servant
>

## type of the API
* When requested at the endpoint `/hello/world`
* Respond with an encoded JSON
* The function that returns the value must return a String

The type of the API is the *contract*
As long as the customer makes a request in `/hello/world` it will get back a Json encoded String

> type hwAPI = "hello" :> "world" :> Get '[JSON] String

Now define the "function" that will actually respond to the request.

> sayHello :: String
> sayHello = "Hello World!"

Let's now associate the function with the API.
the type `Server XXXX` comes from Servant and it must be specified to the API

> hwServer :: Server hwAPI
> hwServer = sayHello

Now we need to add a little bit of boilerplate to guide the type inference

> hwProxy :: Proxy hwAPI
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
$ stack build
$ stack exec hello-world
```
