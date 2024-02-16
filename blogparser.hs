import Data.Char
import Control.Applicative
import Data.Map qualified as M
import System.FilePath


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

notSpecialP = spanP (isAlphaNum)

noBraceP = Parser p
    where p [] = Nothing
          p (c:cs) = case c of 
                '}' -> Nothing
                '{' -> Nothing
                otherwise -> Just (cs, [c])

parseEnclosingBraces :: Parser String
parseEnclosingBraces = (:) <$> (charP '{') <*> ((++) <$> parseCommand <*> stringP "}")

parseCommand :: Parser String
parseCommand = concat <$> many (parseEnclosingBraces <|> noBraceP)

newCommandP :: Parser (String, String)
newCommandP = (\key value -> ('\\':key, value)) <$> (stringP "\\newcommand{\\"
                *> parseCommand <* (stringP "}{"))
                <*> (parseCommand<* (charP '}') <* ignoreLinebreak)

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


theoremEnvironments = M.fromList [("thm", "Theorem"), ("lemm", "Lemma"), ("defn", "Definition"), ("prop", "Proposition"), ("rem", "Remark"), ("fact", "Fact"), ("claim", "Claim"), ("proof", "Proof")]

envP :: String -> Parser HtmlCode
envP env = Parser $ \input -> do 
    (remaining, _) <- runParser (stringP $ "\\begin{" ++ env ++ "}") input
    (rest, inEnv) <- runParser blogP remaining
    (finalRest, _) <- runParser (stringP $ "\\end{" ++ env ++ "}") rest
    return (finalRest, Environment env inEnv)

theoremP :: Parser HtmlCode
theoremP = asum (M.mapWithKey (\key value -> envP key) theoremEnvironments)

envEndP :: String -> Parser String
envEndP env = stringP $ "\\end{" ++ env ++ "}"

theoremEndP :: Parser String
theoremEndP = asum (M.mapWithKey (\key value -> envEndP key) theoremEnvironments)

flipP :: Parser a -> Parser ()
flipP (Parser p) = Parser $ \input -> case p input of 
                            Just _ -> Nothing
                            Nothing -> Just (input, ())

newParagraphP :: Parser HtmlCode
newParagraphP = Parser $ \input -> do 
            (remaining, _) <- runParser (stringP "\n\n") input   
            let (rest, inner) = case runParser (waitForStringP "\n\n") remaining of
                            Nothing -> ("", remaining)
                            Just (rest', inner') -> (rest', inner')
            (nothing, parsedInner) <- runParser blogP inner
            return (rest, P parsedInner)


notSpecialHtmlP :: Parser HtmlCode
notSpecialHtmlP  = Parser p
    where p input = case runParser notSpecialP input of
                Just (_, "") -> Nothing
                Just (s, t) -> Just (s, Inner t) 
                Nothing -> Nothing

ignoreLinebreak :: Parser ()
ignoreLinebreak = Parser p
    where p [] = Just ("", ())
          p input@(c:cs) = case c of 
                    '\n' -> Just (' ':cs, ())
                    otherwise -> Just (input, ())

anyHtmlP :: Parser HtmlCode
anyHtmlP = Parser p
        where p "" = Nothing
              p (c:cs) = Just (cs, Inner [c])

oneStepP :: Parser HtmlCode
oneStepP = (flipP theoremEndP) *> (newParagraphP <|> (theoremP <|> notSpecialHtmlP <|> anyHtmlP))

chainHtmlALAP :: Parser HtmlCode -> Parser HtmlCode
chainHtmlALAP parser = pure More <*> many parser 


blogP :: Parser HtmlCode
blogP = chainHtmlALAP oneStepP



toHtml :: HtmlCode -> String
toHtml (P code) = "<p>" ++ toHtml code ++ "</p>\n"
toHtml (Environment env code) = "<div style=\"display: flex; justify-content: flex-start;\"><b style=\"padding: 0 4px;flex: 0;\">"
                              ++ theoremEnvironments M.! env 
                              ++ "</b><span style=\"flex: 1;\">" 
                              ++ toHtml code
                              ++ "</span>"
                              ++ box
                              ++"</div>\n"
                    where box = if env == "proof" then "<span style=\"margin-top: auto\">$\\Box$</span>" else ""
toHtml (Inner str) = str
toHtml (More []) = ""
toHtml (More (h:hs)) = toHtml h ++ (toHtml (More hs))

blogToHtml :: String -> String
blogToHtml blog = case runParser blogP (preprocess blog) of
    Nothing -> "Something went wrong :("
    Just (_,htmlCode) -> toHtml htmlCode


parseBlogFile :: String -> IO ()
parseBlogFile path = do
    input <- readFile path
    writeFile ("parsedBlog\\" ++ takeBaseName path ++ "parsed.txt") (blogToHtml input)


main = do
    input <- readFile "testblog.txt"
    let parsed = blogToHtml input
    print $ preprocess input
    writeFile "testparsed.txt" parsed
    print parsed