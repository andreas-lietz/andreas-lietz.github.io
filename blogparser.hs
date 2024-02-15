import Data.Char
import Control.Applicative
import Data.Map qualified as M


type NewCommands = M.Map String String

newtype Parser a = Parser {runParser :: String -> Maybe (String, a)}

data HtmlCode = P HtmlCode | Environment String HtmlCode | Inner String | More [HtmlCode] deriving (Show)

data Begin = Begin String String deriving (Show)

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

instance Alternative Parser where
    empty = Parser (const Nothing)
    (Parser p) <|> (Parser q) = Parser (\input -> (p input) <|> (q input))

charP :: Char -> Parser Char
charP c = Parser p
        where p [] = Nothing
              p (d:ds) = if d == c then Just (ds, c) else Nothing 
            
stringP :: String -> Parser String
stringP = traverse charP

spanP :: (Char -> Bool) -> Parser String
spanP cond = Parser (\input -> 
                        let (met, failed) = span cond input
                        in Just (failed, met))

waitForStringP :: String -> Parser String
waitForStringP stopStr = Parser p 
    where   p [] = Nothing
            p input@(c:cs) = case runParser (stringP stopStr) input of
                            Just _ -> Just (input, "")
                            Nothing -> do
                                 (remaining, cut) <- p cs
                                 return (remaining, c:cut) 

specialChars = ['\\', '{']

notSpecialP = spanP (\x -> x `notElem` specialChars)

notClosingBraceP = spanP (/= '}')

newCommandP :: Parser (String, String)
newCommandP = (\key value -> ('\\':key, value)) <$> (stringP "\\newcommand{\\"
                *> notClosingBraceP <* (stringP "}{"))
                <*> (notClosingBraceP <* (charP '}'))

anyCharP = Parser p 
    where p [] = Nothing
          p (c: cs) = Just (cs, c:[])
 

collectNewCommandsP :: Parser (NewCommands, String)
collectNewCommandsP =  Parser p 
    where p "" = Just ("", (M.empty, ""))
          p s = case runParser newCommandP s of
            Just (rest, (key, value)) -> do
                                    (nothing, (commands, leftover)) <- p rest 
                                    return (nothing, (M.insert key value commands, leftover))
            Nothing -> do
                (rest, char) <- runParser anyCharP s
                (nothing, (commands, leftover)) <- p rest
                return (nothing, (commands, char ++ leftover))

ws = spanP isSpace

notAlphaNumCharCheckP = Parser p 
    where p [] = Nothing
          p (c:cs) = if isAlphaNum c then Nothing else Just (c:cs, c:[])

breakChars = ['{', '}', '$', '[', ']']

replaceByP :: String -> String -> Parser String
replaceByP key value = stringP key *> pure value <* notAlphaNumCharCheckP

replaceCommandsP :: NewCommands -> Parser String
replaceCommandsP commands = asum (M.mapWithKey replaceByP commands)

preprocess :: String -> String
preprocess s = case runParser preprocP leftover of
                Just (nothing, processed) -> concat processed
                Nothing -> "Fail!"
    where Just (nothing, (commands,leftover)) = runParser collectNewCommandsP s
          preprocP =  many $ replaceCommandsP commands <|> anyCharP


theoremEnvironments = M.fromList [("thm", "Theorem"), ("lemm", "Lemma"), ("defn", "Definition"), ("prop", "Proposition"), ("rem", "Remark"), ("fact", "Fact"), ("claim", "Claim")]

envP :: String -> Parser HtmlCode
envP env = Parser $ \input -> do 
    (remaining, _) <- runParser (stringP $ "\\begin{" ++ env ++ "}") input
    (rest, inEnv) <- runParser blogP remaining
    (finalRest, _) <- runParser (stringP $ "\\end{" ++ env ++ "}") rest
    return (finalRest, Environment env inEnv)

theoremP :: Parser HtmlCode
theoremP = asum (M.mapWithKey (\key value -> envP key) theoremEnvironments)
        
noEndP = Parser p 
    where p s = case runParser (stringP "\\end{") s of
                    Just _ -> Nothing
                    Nothing -> Just (s, ())

newParagraphP :: Parser HtmlCode
newParagraphP = Parser $ \input -> do 
            (remaining, _) <- runParser (stringP "\n\n") input   
            let (rest, inner) = case runParser (waitForStringP "\n\n") remaining of
                            Nothing -> ("", remaining)
                            Just (rest', inner') -> (rest', inner')
            (nothing, parsedInner) <- runParser blogP inner
            return (rest, P parsedInner)


notSpecialHtmlP :: Parser HtmlCode
notSpecialHtmlP  = (\t-> Inner t) <$> notSpecialP

anyHtmlP :: Parser HtmlCode
anyHtmlP = Parser p
        where p "" = Nothing
              p (c:cs) = Just (cs, Inner [c])

blogP :: Parser HtmlCode
blogP = Parser $ \input -> case runParser noEndP input of 
                Nothing -> Just (input, Inner "")
                Just (input', _) -> do 
                    (remaining, parsed) <- runParser ((newParagraphP <|> theoremP <|> notSpecialHtmlP <|> anyHtmlP)) input'
                    if null remaining 
                        then return ("", parsed)
                        else do 
                            (nothing, remainingParsed) <- runParser blogP remaining
                            return (nothing, More [parsed, remainingParsed]) 



toHtml :: HtmlCode -> String
toHtml (P code) = "<p>" ++ toHtml code ++ "<\\p>\n"
toHtml (Environment env code) = "<div style=\"display: flex;\"><b style=\"padding: 0 4px;flex: 1 0 auto;\">"
                              ++ theoremEnvironments M.! env 
                              ++ "</b><span>" 
                              ++ toHtml code
                              ++ "</span></div>\n"
toHtml (Inner str) = str
toHtml (More []) = ""
toHtml (More (h:hs)) = toHtml h ++ (toHtml (More hs))

blogToHtml :: String -> String
blogToHtml blog = case runParser blogP (preprocess blog) of
    Nothing -> "Something went wrong :("
    Just (_,htmlCode) -> toHtml htmlCode


main = do
    input <- readFile "testblog.txt"
    let parsed = blogToHtml input
    print $ preprocess input
    writeFile "testparsed.txt" parsed
    print parsed