{-# LANGUAGE DeriveFunctor #-}
module BasicParser where

import Json (
    JValue(..)
    )

--https://hasura.io/blog/parser-combinators-walkthrough/


--type Parser a = String -> Either ParseError a

--an error will have a string that describes what was expected
--and another string that explains what we got instead
--derive show so that this can be printed to the ghci prompt
--this is what I coded when I was reading along the article
--data ParseError = ParseError String String deriving Show
--this is from the code that the author uploaded to a gist
data ParseError = ParseError {
    errExpected :: String
    , errFound :: String
} deriving Show

--I was really confused about how to run this because
--I thought that the runParser function was 'attached' to a parser
--but it isn't. Also, it was good to read about when to use newtype over
--data
--derive functor to allow for <$> (fmap) and  return and bind and such -> I need to write this from
-- <$ (replaces all elements somehow, need to study this one. never seen it before)
--scratch sometime
newtype Parser a = Parser {
    runParser :: String -> (String, Either ParseError a)
} deriving (Functor)

--this part was not mentioned in the article
--we need to do this to allow us to use pure and fmap
instance Applicative Parser where
    pure c = Parser $ \s -> (s, Right c)
    --implement fmap for chaining
    pf <*> pa = Parser $ \s -> case runParser pf s of --run parser one
        --if it succeeds and gives us a right value of a parser, and a modified input stream
        --then we need to call parser 2 on the modified input stream
        --which could be done recursively for the entirety of a list of parsers (like mapping)
        (s', Right f) -> fmap f <$> runParser pa s'
        --if it failed, then we need to propagate the error and not run the second parser
        (s', Left e) -> (s', Left e)

--we need to be a monad to allow for >>= and therefore do notation
instance Monad Parser where
    --some parser a and what is f???
    --this is a function declaration for the operator (>>=) and it's two arguments
    pa >>= f = Parser $ \s -> case runParser pa s of
        --run the parser and forward the remaining stream to the next parser
        --in the chain
        (s', Right a) -> runParser (f a) s'
        --parsing failed so we return an error and do not continue chaining
        (s', Left e) -> (s', Left e)


--I guess that runParser isn't a function in the outer scope even though it looks like one
--because of the newtype declaration, if you create a parser it is required that you
--provide an 'implementation' for the runParser 'interface'
--and when we defined 'any' and 'eof', we didn't need to write something like
--any = Parser { runParser = case input of {[] -> ("", Right()); somethingElse -> ...}}
--because the newtype declaration only allows for one 'argument' to the type constructor
--so the compiler realizes that the anonymous functions in 'any' and 'eof' exist to satisfy
--the runParser 'interface'

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

--to make chaining parsers together easier, this is the non-monad version
andThen :: Parser a -> (a -> Parser b) -> Parser b
parserA `andThen` f = Parser $ \input -> case runParser parserA input of
    (restOfInput, Right a) -> runParser (f a) restOfInput
    (restOfInput, Left someError) -> (restOfInput, Left someError)


--2022-11-01 this is full of errors because we haven't defined
--the articles JSON parsers yet
--this is the version that doesn't allow us to do do notation
--jsonEntry :: Parser (String, JValue)
--jsonEntry =
--    --parse a json string
--    jsonString `andThen` \key ->
--        --parse a single colon -> lambda has a dash parameter because we don't need the colon
--        char ':' `andThen` \_ ->
--            --parse a json value
--            jsonValue `andThen` \value ->
--            -- create a constant parser that consumes no input <- Why?
--            constParser (key, value)
