module BasicParser where

--https://hasura.io/blog/parser-combinators-walkthrough/


--type Parser a = String -> Either ParseError a

--an error will have a string that describes what was expected
--and another string that explains what we got instead
--derive show so that this can be printed to the ghci prompt
data ParseError = ParseError String String deriving Show

--I was really confused about how to run this because
--I thought that the runParser function was 'attached' to a parser
--but it isn't. Also, it was good to read about when to use newtype over
--data
newtype Parser a = Parser {
    runParser :: String -> (String, Either ParseError a)
}

--any and eof correspond to the constructors of a list
--eof is like the empty list,
--and any means that there is one or more inputs left (a list with content in it)
any :: Parser Char
any = Parser $ \input -> case input of
    --we used up input x, and inputs xs are still remaining
    (x:xs) -> (xs, Right x)
    --no input means that the parser failed
    [] -> ("", Left $ ParseError "expected any character" "got end of the input")

eof :: Parser ()
eof = Parser $ \input -> case input of
    [] -> ("", Right ())
    somethingElse -> ("", Left $ ParseError "expected end of file (eof)" ("got:" ++ somethingElse))
