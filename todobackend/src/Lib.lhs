TodoBackend
===========================================

Now let's create something a little bit more complicated a TODO List application.
Fortunately for us there is: http://www.todobackend.com/
which:

<blockquote>
"This website defines a simple web API in the form of a todo list and users can create their own identical APIs using various tech stacks. This website then provides a spec runner to verify that the user implementation is identical to the base implementation."
</blockquote>

1. Find the requirements of the API
-------------------------------------------------------
According to [todobackend.com](http://www.todobackend.com/contribute.html)
<blockquote>
It's pretty simple to implement the Todo-Backend API.
The entire API consists of about 5 distinct operations:
  * create a todo
  * view a todo
  * modify a todo
  * list all todos
  * delete all todos
</blockquote>

From this highlevel overview seems pretty obvious that we will need a `Todo` type
and a `[Todo]` list of Todos type.

But how is a Todo composed?
The answer [lies in the tests](https://github.com/TodoBackend/todo-backend-js-spec/blob/master/js/specs.js). How ever it is all over the place we can find a more concise explanation of what is expected in this specification written in [swagger (OpenAPI)](https://github.com/IBM-Swift/todolist-swagger/blob/master/swagger.yaml). In this document we can find the specification we are looking for:


```yaml
definitions:
  todo:
    type: object
    required:
      - url
      - user
      - title
      - id
      - order
      - completed
    properties:
      url:
        type: string
      user:
        type: string
      title:
        type: string
      id:
        type: string
      order:
        type: integer
      completed:
        type: boolean
  errorModel:
    type: object
    required:
      - code
      - message
    properties:
      code:
        type: integer
        format: int32
      message:
        type: string
  todobody:
    properties:
      data:
        type: object
        properties:
          title:
            type: string
          order:
            type: integer
          completed:
            type: boolean
```

Looking at this document and the previous highlevel overview we can create our project.


0. Let's create the project
---------------

So let's create a new project now we will use the "servant" template of stack.

```shell
$ stack create new todobackend-hss servant
```
That will create a default project for us.

you can see the structure it crated with
```json
$ cd todobackend-hss
$ ls -l
```
As you can see is pracitcally the same except it has creatde the `app` directory
with a `Main.hs` file inside of it.

We will write all of our code in `./src/Lib.hs`
Go and open it with your favorite editor.

The first few lines are PRAGMAS and imports we will require so those stay the same.

>{-# LANGUAGE DataKinds       #-}
>{-# LANGUAGE TemplateHaskell #-}
>{-# LANGUAGE TypeOperators   #-}
>
>module Lib
>    ( startApp
>    ) where
>
>import           Control.Monad.IO.Class       (liftIO)
>import Data.Aeson
>import Data.Aeson.TH
>import Network.Wai
>import Network.Wai.Handler.Warp
>import Servant
>


Given the paths in [todobacken-swagger](https://github.com/TodoBackend/todo-backend-js-spec/blob/master/js/specs.js), We can define the API as follows:


Return all todos of the User
--------------------------------------------------------------------------------
```yaml
paths:
  /:
    get:
      description: Returns all todos of the user
      operationId: findTodos
      produces:
        - application/json
        - application/xml
        - text/xml
        - text/html
      responses:
        '200':
          description: todos response
          schema:
            type: array
            items:
              $ref: '#/definitions/todo'
        default:
          description: unexpected error
          schema:
            $ref: '#/definitions/errorModel
```

This operation seems easy to translate:

```haskell ignore
type API = Get '[JSON] [Todo]
```


Create a New Todo
--------------------------------------------------------------------------------
Now on to the next operation

```yaml
    post:
      description: Creates a new todo item.  Duplicates are allowed
      operationId: addTodo
      produces:
        - application/json
      parameters:
        - name: todoitem
          in: body
          description: todo item to be added to todoList
          required: true
          schema:
            $ref: '#/definitions/todobody'
      responses:
        '200':
          description: the added todo item
          schema:
            $ref: '#/definitions/todo'
        default:
          description: unexpected error
          schema:
            $ref: '#/definitions/errorModel'
```

In this description the interesting bits is that in the `request body` is going to come a `todo item to be added to the list`. So we acknowledge that as part of our API.

```haskell ignore
         :<|> ReqBody '[JSON] TodoBody :> Post '[JSON] Todo
```

Delete the list of ToDos
--------------------------------------------------------------------------------
The next part of the spec says that the delete operation should be enough to delete the entire list.

```yaml
    delete:
      description: Deletes all todo items of current user
      operationId: deleteTodos
      produces:
        - application/json
      responses:
        default:
          description: unexpected error
          schema:
            $ref: '#/definitions/errorModel'
```

So it says that the delete method of the HTTP specification must return a Json a json of what?
An empty list, since all the Todos for the current user where deleted.

```haskell ignore
         :<|> Delete `[JSON] []
```


Implementing the next end-point
--------------------------------------------------------------------------------
```yaml
  /api/todos/{todoID}:
    get:
      description: Returns a todo of the user
      operationId: getTodo
      produces:
        - application/json
        - application/xml
        - text/xml
        - text/html
      parameters:
        - in: path
          name: todoID
          description: todo documentID
          required: true
          type: string
      responses:
        '200':
          description: todo item
          schema:
            $ref: '#/definitions/todo'
        default:
          description: unexpected error
          schema:
            $ref: '#/definitions/errorModel'
```

The interesting part is that now it claims the parameter should be in path
And that is pretty easy to interperet with `Capture`

```haskell ignore
         :<|> "api":>"todos":>Capture "todoID" String :> Get '[JSON] Todo
```

Updating a todo item
--------------------------------------------------------------------------------
```yaml
    post:
      description: Updates a todo item
      operationId: updatePetById
      produces:
        - application/json
        - application/xml
        - text/xml
        - text/html
      parameters:
        - name: todoID
          in: path
          description: ID of todo document
          required: true
          type: string
        - name: todoitem
          in: body
          description: todo item details to be updated
          required: true
          schema:
            $ref: '#/definitions/todobody'
      responses:
        '200':
          description: the updated todo item
          schema:
            $ref: '#/definitions/todo'
        default:
          description: unexpected error
          schema:
            $ref: '#/definitions/errorModel'
```

So it is basically the same except for the verb that is going to use

```haskell ignore
         :<|> "api":>"todos":>Capture "todoID" String :> ReqBody '[JSON] TodoBody :> Post '[JSON] Todo
```


Delete a specific Todo
--------------------------------------------------------------------------------
```yaml
    delete:
      description: deletes a todo item from todolist
      operationId: deleteTodo
      parameters:
        - name: todoID
          in: path
          description: todo documentID
          required: true
          type: string
      responses:
        default:
          description: unexpected error
          schema:
            $ref: '#/definitions/errorModel'

```

Again the only thing that changes is the HTTP verb

```haskell ignore
         :<|> "api":>"todos":>Capture "todoID" String :> Delete '[JSON] []
```

The complete API
--------------------------------------------------------------------------------
The complete API in servant looks like this:

```haskell ignore

>type API = Get '[JSON] [Todo]
>         :<|> ReqBody '[JSON] TodoBody :> Post '[JSON] Todo
>         :<|> Delete '[JSON] []
>         :<|> "api":>"todos":>Capture "todoID" String :> Get '[JSON] Todo
>         :<|> "api":>"todos":>Capture "todoID" String :> Post '[JSON] Todo
>         :<|> "api":>"todos":>Capture "todoID" String :> Delete '[JSON] []

```
A lot shorter than the swagger equivalent but still not good enough, we can reduce it even further

Using the attribuet that APIs are composable

type todoItem = "api" :> "todos" :> Capture "todoID" String

type API = Get '[JSON] [Todo]
         :<|> ReqBody '[JSON] TodoBody :> Post '[JSON] Todo
         :<|> Delete `[JSON] []
         :<|> todoItem :> Get '[JSON] Todo
         :<|> todoItem :> ReqBody '[JSON] TodoBody :> Post '[JSON] Todo
         :<|> todoItem :> Delete '[JSON] []


Defining the Data types
--------------------------------------------------------------------------------
From the API declaration we can see that we need 3 datatpes:

* List of Todos
* Todo
* Todobody

let's start with the later, in the swagger specification it looks like this:

```yaml
  todobody:
    properties:
      data:
        type: object
        properties:
          title:
            type: string
          order:
            type: integer
          completed:
            type: boolean
```

Which Haskell looks like this

>
>data TodoBody = TodoBody {title :: String
>   , order :: Maybe Integer
>   , completed :: Maybe Bool
>   } deriving (Eq, Show)
>

Now the Todo type

```yaml
definitions:
  todo:
    type: object
    required:
      - url
      - user
      - title
      - id
      - order
      - completed
    properties:
      url:
        type: string
      user:
        type: string
      title:
        type: string
      id:
        type: string
      order:
        type: integer
      completed:
        type: boolean
```

>data Todo = Todo { body :: TodoBody
>   , url :: String
>   , user :: String
>   } deriving (Eq, Show)

And that's it! those are the only data types we need..

Now detelete the previes User data type declaration, that was there.

```json
data User = User
  { userId        :: Int
  , userFirstName :: String
  , userLastName  :: String
  } deriving (Eq, Show)

```

Designing the functions that will implement the API
-------------------------------------------
Based on the swagger file we can make the follwing function declarations`

>
>findTodos :: [Todo]
>findTodos = [undefined]
>
>addTodo :: TodoBody -> Todo
>addTodo = undefined
>
>deleteTodos = []
>
>getTodo :: Integer -> Todo
>getTodo = []
>
>updatePetById :: Integer->[Todo]
>updatePetById = undefined
>
>deleteTodo :: Integer -> [Todo]
>deleteTodo = undefined
>

Now we need to associate our "functions" with the API

>server :: Server API
>server = liftIO $ print $ findTodos
>   :<|> liftIO $ print $ addTodo
>   :<|> liftIO $ print $ deleteTodo
>   :<|> liftIO $ print $ getTodo
>   :<|> liftIO $ print $ updatePetById
>   :<|> liftIO $ print $ deleteTodo
>

>
>startApp :: IO ()
>startApp = run 8080 app
>
>app :: Application
>app = serve api server
>
>api :: Proxy API
>api = Proxy
>
>


Testing our development so far
--------------------------------------------------------------------------------

We have written a lot of code let's check if it compiles.

```shell
$ stack build
```
