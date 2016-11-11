# Hello Servant

## Setting Up a New Project

To create a new Haskell-Servant api we simply need to use stack.

```shell
$ stack new hello-world servant
```

This command tells `stack` to create a `new` project named `hello-world` using the `servant` template.

A template is a predefined structure for a project. It's an easy way to Jumpstart our project.

Once stack is finished you will have a project structure that looks like this:

``` shell
$ find . -type f -not -path "*.stack-work/*" | sort
./app/Main.hs
./hello-world.cabal
./LICENSE
./Setup.hs
./src/Lib.hs
./stack.yaml
./test/Spec.hs
```

### Running the Example API
Using `stack` with the `servant` template creates a default API for us.
In order to check it is necesary to:

``` shell
cd hello-world
stack build
stack exec hello-world-exe
```

And then in your browser you visit the url: http://localhost:8080/users/
And it will show you the response.

``` json
[{"userId":1,"userFirstName":"Isaac","userLastName":"Newton"}
,{"userId":2,"userFirstName":"Albert","userLastName":"Einstein"}]
```

This is a pretty basic api
* You made a Get Request (when your browser visited http://localhost:8080/users/)
* and got a JSON response.


### Modify .cabal file (Optional)
The next step is to modify the `hello-world.cabal` file.
This file has information about our application that would be useful when we decide to make it public.

### Modify hosts file (optional)
It is nice to test our api using the domain that will be used in production.
In order to make that we can "fake it" using the /etc/hosts/file.

## Hello World API

### Description

The hello world api offers two endpoints:

    * `/hello/world` will respond with the message "Hello World!"
    * `/hello/{name}` will respond with the message "Hi {name}, !"

### API type

Haskell is a strongly typed language and Servant allows us to create strongly type APIs
So the end point `/hello/world` in servant is represented as:

``` haskell ignore
type hwAPI = "hello" :> "world" :> Get `[PlainText] String
```
The first half is just the route `/hello/world` translated as `"hello" :> "world"`
The Second half  ` Get '[PlainText] String` is the interesting thing.
It means that we are going to return the answer the
`Get` request with a `'[PlainText]` response and the Function that will answer that
must return a `String`

The second route `/hello/{name}` can then defined as:

``` haskell
type hwAPI
    = "hello" :> "world" :> Get `[PlainText] String
    :<|> "hello" :> Capture "Name" String :> Get `[PlainText] String
```

The `:<|>` simbol means "or" mening the first route or the second.
`Capture "name" String` means that the next part of the request is going to be named "name" and it should be interpreted as a string.
and it will return a `[PlainText]` But the function that responds to this request mus return a `String`.


### API Contract

Notice how the `type hwAPI` completely specifies what client *must* provide in order to get a response.
And also specifies what the answer will respond and in what format.
With this contract both the client and the server know their responsabilites and
and the front and backend teams can go to work.

### Responding to API calls.

Now that we have defiend the `type hwAPI` we need to actually link it to the functions that will implement it.

    First we specify the type of the  function: `helloServer` of type `Server hwAPI` this is a function that will be our execute the functions that actually respond to the API request.

``` haskell ignore
hwServer :: Server hwAPI

```

    Second we specify the functions that will respond to the API calls.
    *This functions must be in the same order as the `type hwAPI`*

``` haskell
hwServer :: Server hwAPI
hwServer = sayHelloWorld :<|> sayHi --This functions will be defined further in the tutorial
```

Now in order to create an *actual* web server we need to provide the following code

``` haskell
-- Create a proxy for the API... this is boilerplate.
hwProxy :: Proxy hwAPI
hwProxy = Proxy


-- 'serve' comes from servant and hands you a WAI Application,
-- which you can think of as an "abstract" web application,
-- not yet a webserver.

hwApp :: Application
hwAPP = serve hwProxy hwServer

-- And we’re done! Let’s run our webservice on the port 8080.

main :: IO ()
main = run 8080 hwApp
```
