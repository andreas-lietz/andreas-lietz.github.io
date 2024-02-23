module BasicParsing where


import Data.Char
import Control.Applicative
import Data.List
import qualified Data.Map as M


--typedefs
newtype Parser a = Parser {runParser :: String -> Maybe (String, a)}

--instancing Parser as Functor,Applicative,Monad,Alternative
instance Functor Parser where
    fmap f (Parser p) = Parser (\input -> do
                                (s, x) <- p input
                                return (s, f x))

instance Applicative Parser where
    pure x = Parser (\input -> Just (input, x))
    (Parser funcP) <*> (Parser argP) = Parser (\input -> do
                                (s0, f) <- funcP input
                                (s1, x) <- argP s0
                                return (s1, f x))
        
instance Monad Parser where
    (Parser p) >>= f = Parser $ \input -> do
                                (s, x) <- p input
                                let (Parser q) = f x
                                q s

instance Alternative Parser where
    empty = Parser (const Nothing)
    (Parser p) <|> (Parser q) = Parser (\input -> (p input) <|> (q input))



--basic parser combinators

condP :: (String -> Bool) -> Parser a -> Parser a
condP cond (Parser p) = Parser $ \input -> if cond input then p input else Nothing

condCharP :: (Char -> Bool) -> Parser a -> Parser a
condCharP cond (Parser p) = Parser $ \input -> case input of 
                            "" -> Nothing
                            (c:cs) -> if cond c then p input else Nothing

ifElseP :: (String -> Bool) -> Parser a -> Parser a -> Parser a
ifElseP cond (Parser trueP) (Parser falseP) = Parser $ \input -> 
                                if cond input then trueP input else falseP input

flipP :: Parser a -> Parser ()
flipP (Parser p) = Parser $ \input -> case p input of 
                            Just _ -> Nothing
                            Nothing -> Just (input, ())


waitForCondP :: (String -> Bool) -> Parser String 
waitForCondP cond = concat <$> many (ifElseP cond noP anyCharP) <* (condP cond (pure ()))


untouchedP :: Parser a -> Parser String
untouchedP parser = concat <$> many (parser *> (pure "") <|> anyCharP)

--basic parsers
noP :: Parser a
noP = Parser $ \input -> Nothing

anyCharP = Parser p 
    where p [] = Nothing
          p (c: cs) = Just (cs, c:[])
        
nonEmptyP = condP (/= "") $ pure ()


consumeCharP :: Parser String
consumeCharP = anyCharP *> (pure "")


charP :: Char -> Parser Char
charP c = (\s -> s !! 0) <$> condCharP (==c) anyCharP
            
stringP :: String -> Parser String
stringP = traverse charP

spanP :: (Char -> Bool) -> Parser String
spanP cond = Parser (\input -> 
                        let (met, failed) = span cond input
                        in Just (failed, met))

ws = spanP isSpace

notSpecialP = concat <$> (some $ condCharP (\c -> isAlphaNum c || isSpace c) anyCharP)

ignoreLinebreakP= (charP '\n' *> pure ()) <|> pure () 

--handling curly braces

noBraceP = condP (\s -> not $ or $ map (flip isPrefixOf s) ["{", "}"]) anyCharP

enclosingBracesP :: Parser String
enclosingBracesP = (:) <$> (charP '{') <*> ((++) <$> inBracesP <*> stringP "}")

inBracesP :: Parser String
inBracesP = concat <$> many (enclosingBracesP <|> noBraceP)


--replacing logic
notAlphaNumCharCheckP = condP (\s -> case s of {"" -> True; (c:cs) -> not $ isAlphaNum c}) $ pure ""

replaceByP :: String -> String -> Parser String
replaceByP key value = stringP key *> pure value <* notAlphaNumCharCheckP

fullReplaceByP :: String -> String -> Parser String 
fullReplaceByP key value = concat <$> (many $ replaceByP key value <|> anyCharP)


--extractions 
extractAndRememberKeyP :: String -> Parser (String, String)
extractAndRememberKeyP key = (\s -> (key, s)) <$> ((stringP $ "\\" ++ key ++ "{") *> inBracesP <* charP '}' <* ws )

extractManyP :: [String] -> Parser (M.Map String String)
extractManyP keys = M.fromList <$> (many (asum (map extractAndRememberKeyP keys) <|> (consumeCharP *> (pure ("","")))))

extractFromEnvironmentP :: String -> Parser String
extractFromEnvironmentP env = (stringP $ "\\begin{" ++ env ++ "}") *> (waitForCondP (isPrefixOf end)) <* (stringP end) 
    where end = "\\end{" ++ env ++ "}"

