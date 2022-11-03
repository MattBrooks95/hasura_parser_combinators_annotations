module MonadicParser where
import Data.Char (
    isSpace
    , isDigit
    )
import BasicParser (
    ParseError(..)
    , any
    , Parser(..)
    , runParser
    , eof
    )
import Json (
    JValue(..)
    )

import Data.HashMap.Strict as HM
import Data.Foldable (
    foldr
    )

--newtype Parser a = Parser {
--    runParser :: String -> (String, Either ParseError a)
--}

--this has been rewritten to use do notation, since we made the parser monadic
jsonEntry :: Parser (String, JValue)
jsonEntry = do
    key <- jsonString
    _ <- char ':'
    value <- jsonValue
    return (key, value)

--'state' here is just the input stream of characters
--we aren't doing any book keeping about what happens as the parsing continues,
--so looking at whether the input stream was changed or not is our 'state'
try :: Parser a -> Parser a
try p = Parser $ \state -> case runParser p state of
    (_newState, Left err) -> (state, Left err)
    success -> success --here, 'success' must be the value as it is after parser p succeeded?

--this is a conditional, you use it to build a parser that tries to parse something
--and if the parse fails, then it leaves the input stream as is, and gives control
--flow to the next parser, that's why we need try
satisfy :: String -> (Char -> Bool) -> Parser Char
satisfy description predicate = try $ do
    c <- BasicParser.any
    if predicate c
        then return c
        else parseError description [c]

--create a parse error, as a parser so that it can be the return type of a function
--like satisfy
parseError :: String -> String -> Parser a
parseError expected found = Parser $ \input -> (input, Left $ ParseError expected found)

--take parser a1, and parser a2, and then return the one that succeeds
--or, a ParseError on failure
(<|>) :: Parser a -> Parser a -> Parser a
p1 <|> p2 = Parser $ \state -> case runParser p1 state of
    (state', Left err)
        --s' == s means that the parser failed, but also didn't take any characters
        --out of the input stream, so we can run parser 2
        | state' == state -> runParser p2 state
        | otherwise -> (state', Left err)
    --if parser a1 succeeded, then we throw away parser 2 and continue on to other parsers
    success -> success

--tries a list of parsers and short circuits when one succeeds
choice :: String -> [Parser a] -> Parser a
choice description = Data.Foldable.foldr (<|>) noMatch
    where noMatch = parseError description "no match"

--many matches any number of chars (* from regex, matches 0 or more)
--many1 matches 1 or more (like + in regex)
many, many1 :: Parser a -> Parser [a]
many p = many1 p <|> return []
many1 p = do
    first <- p
    rest <- many p
    return (first:rest)

--match repeated occurences of a given parser with a separator between them
--parser a matches the first element
-- parser s matches the separator (its results are thrown out, no reason to keep the commas in 1,2,3,4)
--sepBy, sepBy1 :: Parser a -> Parser s -> Parser [a]
sepBy p s = sepBy1 p s <|> return []
sepBy1 p s = do
    first <- p
    -- the arg to 'many' says "a parser that matches and then throws away a separator, and then runs the next parser)
    -- which could then do the same recursively
    rest <- many (s >> p)
    return (first:rest)

--char :: Char -> Parser Char
char c = satisfy [c] (== c)

--these isSpace and isDigit functions come from Data.Char
space :: Parser Char
space = satisfy "space" isSpace
digit :: Parser Char
digit = satisfy "digit" isDigit

--string :: [Char] -> Parser [Char]
string = traverse char

spaces :: Parser [Char]
spaces = many space
--symbol skips trailing whitespace, the <* operator throws away the white space
--after the symbol
--symbol :: String -> Parser [Char]
symbol s = string s <* spaces

--I don't believe these were actually mentioned in the article
--but the provided code had it
--takes an opening parser, and a closing parser
--and then a parser that parses the contents, then returns a parser that does
--the sum of those parsers
--in the article's code, they use currying to create parsers like 'brackets' or 'braces'
--which already have the opening and closing parsers baked in,
--and then when you call the curried version with a 3rd parser, you can create a new parser
--for that content. You compose the content parser with a pre-made parser for brackets or braces
--but it could be more than brackets or braces for opening and closing, it could be anything
--trying to figure out the type signature for this
--between :: Parser a -> Parser a -> Parser b -> Parser a
--    writing something like this limits the situations in which it can be used
--use cases
--between (char '"') (char '"') (many jsonChar) <* spaces 
--    would be Parser Char -> Parser Char -> Parser[Char] -> Parser Char
--between (string "start") (string "end") (many jsonChar) <* spaces
--    would be Parser [Char] -> Parser [Char] -> Parser [Char] -> Parser [Char]
--the type inference is saying that the type signature is
--Applicative f -> f a1 -> f a2 -> f b -> f a2
--
--this line was written like this, so it was returning the type of the closing parser
--and not the type of the content parser, it should be opening *> parser <* closing
--"opening" *> (throw away opening) "parser" "closing" (throw away closing), leaving the parser
--between opening closing parser = opening *> closing <* parser
between :: Applicative f => f a -> f b -> f a1 -> f a1
between opening closing parser = opening *> parser <* closing

--if you say that it only works on char strings,
--you restrict the situations in which it can be used, unecessarily
--brackets :: Parser [Char] -> Parser [Char]
--brackets :: Parser [Char] -> Parser [Char]
--brackets :: Parser b -> Parser [Char]
brackets = between (symbol "[") (symbol "]")

--if you say that it only works on char strings,
--you restrict the situations in which it can be used, unecessarily
--braces :: Parser [Char] -> Parser [Char]
--braces :: Parser b -> Parser [Char]
braces :: Parser b -> Parser b
braces = between (symbol "{") (symbol "}")

jsonNumber :: Parser[Char]
jsonNumber = read <$> many1 digit

--tries the parsers in the array, and returns the first one that succeeds
-- "true" could be the javascript true, and "false" could be the javascript false
jsonBool :: Parser Bool
jsonBool = choice "JSON boolean"
    [ True <$ symbol "true" --the characters 't' 'r' 'u' 'e'
    , False <$ symbol "false" --the characters 'f' 'a' 'l' 's' 'e'
    ]

jsonString =
    --a string is many characters between double quotes
    --between (char '"') (char '"') (many jsonChar) <* spaces
    between (string "start") (string "end") (many jsonChar) <* spaces
    where
        --a character is any non-quote character
        jsonChar = choice "JSON string character"
            [ try $ '\n' <$ string "\\n" --handling escaped characters (this is only a subset of real json)
            , try $ '\t' <$ string "\\t"
            , try $ '"' <$ string "\\\""
            , try $ '\\' <$ string "\\\\"
            , satisfy "not a quote" (/= '"')
            ]

--json object (key value pairs)
--gets turned into a list of (key, value) tuples, which is then turned into a HashMap
jsonObject = do
    assocList <- braces $ jsonEntry `sepBy` symbol ","
    return $ HM.fromList assocList
    --where
    --    jsonEntry = do
    --        k <- jsonString
    --        _ <- symbol ":" -- need to assign to the underscore here to suppress a compiler warning
    --        v <- jsonValue
    --        return (k, v)

--interesting that it's not '[' jsonValue sepby symbol ']', like it might be in something like
--ANTLR
--the parser combinator is set up so that it's written bracket-bracket-content at the call site
jsonArray = brackets $ jsonValue `sepBy` symbol ","

jsonValue :: Parser JValue
jsonValue = choice "a JSON value"
    [ JObject <$> jsonObject
    , JArray <$> jsonArray
    , JString <$> jsonString
    , JBool <$> jsonBool
    , JNull <$ symbol "null"
    ]

json = spaces >> jsonValue

run :: Parser a -> String -> Either ParseError a
run parser stringInput = snd $ runParser (parser <* eof) stringInput

--(*>) :: Parser a -> Parser b -> Parser b

--spaces *> value
--(<*) :: Parser a -> Parser b -> Parser a
--(<$) :: a -> Parser b -> Parser a
