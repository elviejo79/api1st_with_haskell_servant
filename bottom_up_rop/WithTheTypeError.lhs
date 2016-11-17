Designing for Errors
=========================================================
The previous approach with the Either Monad returns a text
but it is preferably to know what kind of type was generated



So to start we can start modeling the request.

> module Lib2 where
>
> import Data.Char
> import Data.String.Utils (strip)

> import Control.Exception.Safe
> import Control.Exception.Base(evaluate)
>
> import Control.Monad.Error
>
> data Request = Request {userId :: Integer
>                        ,name :: String
>                        ,email :: String
>                        } deriving (Show)
>
>

Desgining for Errors
--------------------------------------------------------
Unhappy paths are requirements too.

Now we will implementthe adavanced feature of using one type to report all errors.

![](http://image.slidesharecdn.com/railway-oriented-programming-slideshare-140312155941-phpapp01/95/railway-oriented-programming-130-638.jpg?cb=1427456657)

In order todo that we will use the approach #5 in [this article](http://www.randomhacks.net/2007/03/10/haskell-8-ways-to-report-errors/).

so we need to import the MonadError

> import Control.Monad.Error
>
and then define our own Error type that will be used to display the kind of error that is needed.

> data ErrorMessages =
>   | NameMustNotBeLargerThan50
>   | NameMustNotBeBlank
>   | EmailMustNotBeBlank
>   | EmailNotValid String
>   | MiscError String

Now we need to say how those types will be displayed

> instance Show Custom Error where
>   show NameMustNotBeBlank =  "Name must not be blank"
>   show EmailMustNotBeBlank = "Email must not be blank"
>   show NameMustNotBeLargerThan50 = " Name must not be bigger than 50 characters"
>   show EmailNotValid email = "Email given is not valid" ++ email
>

And in cake of a complete unknown error:

> instance Error ErrorMessage where
>   noMsg = MiscError "Unknown error"
>   strMsg str = MiscError str

1. Now let's refactor our functions to show the appropriate error
--------------------------------

> -- was: nameNotBlank :: Request -> Either String Request
> nameNotBlank :: Request -> Either ErrorMessages Request
> nameNotBlank Request {name = ""} = Left NameMustNotBeLargerThan50
> nameNotBlank input  = Right input
>
> name50 :: Request -> Either ErrorMessages Request
> name50 input = if length (name input) > 50
>                then Left NameMustNotBeLargerThan50
>                else Right input
>
> emailNotBlank :: Request -> Either ErrorMessages Request
> emailNotBlank Request {email = ""} =  Left EmailMustNotBeBlank
> emailNotBlank input = Right input

Now we can compose this validaton functions in "two track mode"
![](http://image.slidesharecdn.com/railway-oriented-programming-slideshare-140312155941-phpapp01/95/railway-oriented-programming-96-638.jpg?cb=1427456657)

> validateRequest :: Either ErrorMessages Request -> Either ErrorMessages Request
> validateRequest twoTrackInput =
>  twoTrackInput
>  >>= nameNotBlank
>  >>= name50
>  >>= emailNotBlank
>

You can test this functions your self

```shell
$ stack ghci
```
