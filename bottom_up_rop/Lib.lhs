A monad tutorial inspired by Railway Oriented Programming.
=========================================================

Our objective is to update an email as explained here:
![](http://image.slidesharecdn.com/railway-oriented-programming-slideshare-140312155941-phpapp01/95/railway-oriented-programming-12-638.jpg?cb=1427456657)

So to start we can start modeling the request.

> module Lib where
>
> import Data.Char
> import Data.String.Utils (strip)
> import Data.Either()
>
> data Request = Request {userId :: Integer
>                        ,name :: String
>                        ,email :: String
>                        } deriving (Show)
>
>

Fortuntaly the design is already made:
![](http://image.slidesharecdn.com/railway-oriented-programming-slideshare-140312155941-phpapp01/95/railway-oriented-programming-27-638.jpg?cb=1427456657)


1. Bind Example
--------------------------------
Given this functions
![](http://image.slidesharecdn.com/railway-oriented-programming-slideshare-140312155941-phpapp01/95/railway-oriented-programming-91-638.jpg?cb=1427456657)

Representnig them in haskell is pretty straight forward:

> nameNotBlank :: Request -> Either String Request
> nameNotBlank Request {name = ""} = Left "Name must not be  blank"
> nameNotBlank input  = Right input
>
> name50 :: Request -> Either String Request
> name50 input = if length (name input) > 50
>                then Left "Name must not be over 50 characters"
>                else Right input
>
> emailNotBlank :: Request -> Either String Request
> emailNotBlank Request {email = ""} =  Left "Email must not be blank"
> emailNotBlank input = Right input

Now we can compose this validaton functions in "two track mode"
![](http://image.slidesharecdn.com/railway-oriented-programming-slideshare-140312155941-phpapp01/95/railway-oriented-programming-96-638.jpg?cb=1427456657)

> validateRequest :: Either String Request -> Either String Request
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

And in ghci you execute

> goodRequest = (Request 1 "alex" "ALEX@EXAMPLE.COM")
> goodExample :: Either a Request
> goodExample = return goodRequest
> -- >>> validateRequest goodExample
> -- Right (Request {userId = 1, name = "alex", email = "ALEX@EXAMPLE.COM"})
> --
> -- >>> badExample = return (Request 999 "" "")
> -- >>> validateRequest badExample
> -- Left "Name must not be  blank"


Converting one-track functions
------------------------------------------------------------------------
How do we make a two track function of a function than is single track?

> canonicalizeEmail :: Request -> Request
> canonicalizeEmail  request = let
>                              newEmail = strip $ map toLower $ email request
>                              in request {email = newEmail}
>
> twoTrackCanonicalizeEmail :: (Either a Request -> Either a Request)
> twoTrackCanonicalizeEmail = fmap canonicalizeEmail
>

Converting dead-end functions
-------------------------------------------------------------------------
A function that doesn't return anything -- a dead end function

> updateDb :: Request -> IO ()
> updateDb request = do
>   writeFile "/tmp/haskell_file.txt" (show request)
>
