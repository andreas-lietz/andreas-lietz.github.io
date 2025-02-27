import Data.Char
import Control.Applicative
import Data.Map qualified as M
import Data.List
import System.FilePath
import Preprocessing
import BasicParsing

--typedef

data HtmlCode = P HtmlCode | Formatting String HtmlCode | Environment String HtmlCode | Inner String | More [HtmlCode] deriving (Show)

--environment parsing

theoremEnvironments = M.fromList [("thm", "Theorem"), ("lemm", "Lemma"), ("defn", "Definition"), ("prop", "Proposition"), ("cor", "Corollary"), ("rem", "Remark"), ("fact", "Fact"), ("que", "Question"), ("claim", "Claim"), ("proof", "Proof"), ("tldr", "tldr")]

formats = M.fromList [("textbf", "b"), ("textit", "em"), ("section", "h2"), ("subsection", "h4")]

enumerateP :: Parser HtmlCode
enumerateP = chainHtmlALAP (ws *> stringP "\\item " *> blogP)
    

environmentP :: Parser HtmlCode
environmentP = Parser $ \input -> do
    (afterBegin, _) <- runParser (stringP "\\begin{") input
    (afterBeginEnv, env) <- runParser (waitForCondP  (isPrefixOf "}") <* charP '}') afterBegin
    (afterInner, innerHtml) <- if env `elem` ["enumerate", "itemize"] then runParser enumerateP afterBeginEnv else runParser blogP $ "\n\n" ++ afterBeginEnv
    (afterEnd, _) <- runParser  (ws *> (stringP $ "\\end{" ++ env ++ "}")) afterInner
    return (afterEnd, Environment env innerHtml)

--paragraph parsing

addParagraphP :: Parser HtmlCode
addParagraphP = Parser $ \input -> do
            (rest, inner) <- runParser (waitForCondP (\str 
                                -> str == "" 
                                || (or $ map (flip isPrefixOf str) ["\n\n", "\\begin{", "\\end{"] ))) input
            (nothing, parsedInner) <- runParser blogP inner
            if nothing == "" then return (rest, P parsedInner) else Nothing

newParagraphP :: Parser HtmlCode
newParagraphP = stringP "\n\n" *> addParagraphP

formatP :: Parser HtmlCode
formatP = charP '\\' *> (Parser $ \input -> do
            (afterIdent, identifier) <- runParser (waitForCondP (\str -> "{" `isPrefixOf` str)) input
            if not $ identifier `M.member` formats then Nothing else do
                (rest, braces) <- runParser enclosingBracesP afterIdent
                (nothing, parsedInner) <- runParser blogP (tail $ init braces)
                if nothing == "" then return (rest, Formatting identifier parsedInner) else Nothing)

notSpecialHtmlP :: Parser HtmlCode
notSpecialHtmlP  = Inner <$> notSpecialP


anyHtmlP :: Parser HtmlCode
anyHtmlP = Parser p
        where p "" = Nothing
              p (c:cs) = Just (cs, Inner [c])

noBeginEndOrItem :: String -> Bool
noBeginEndOrItem s = not $ or $ map (flip isPrefixOf s) ["\\begin{", "\\end{", "\\item"]

oneStepP :: Parser HtmlCode
oneStepP = (flipP $ stringP "\\end{") *> (newParagraphP <|> formatP <|> (environmentP <|> notSpecialHtmlP <|> (condP noBeginEndOrItem anyHtmlP)))

chainHtmlALAP :: Parser HtmlCode -> Parser HtmlCode
chainHtmlALAP parser = pure More <*> many parser 


blogP :: Parser HtmlCode
blogP = chainHtmlALAP oneStepP

environmentToHtml :: HtmlCode -> String
environmentToHtml (Environment env inEnv)
    | env `M.member` theoremEnvironments = "<div style=\"display: flex; justify-content: flex-start;\"><b style=\"padding: 0 4px;flex: 0;\">"
                              ++ theoremEnvironments M.! env 
                              ++ "</b><span style=\"flex: 1;\">" 
                              ++ toHtml inEnv
                              ++ "</span>"
                              ++ box
                              ++"</div>\n"
    | env == "enumerate" = case inEnv of 
                            More htmlList -> concatMap (\(index, code) -> toHtml (P (More [Inner $ "$(" ++ (show index) ++ ")$ ", code]))) $ zip [1..] htmlList
                            otherwise -> "Error! More expected in enumerate environment!"
    | env == "itemize" = case inEnv of 
                            More htmlList -> concatMap (\(index, code) ->  (toHtml (P (More [Inner "&bull; ", code])))) $ zip [1..] htmlList
                            otherwise -> "Error! More expected in enumerate environment!"  
    | env == "comment" = ""
    | otherwise = "\\begin{" ++ env ++ "}\n" ++ (toHtml inEnv) ++ "\n\\end{" ++ env ++ "}" 
                    where box = if env == "proof" then "<span style=\"margin-top: auto\">$\\Box$</span>" else ""



toHtml :: HtmlCode -> String
toHtml (P code) = "<p>" ++ toHtml code ++ "</p>\n"
toHtml (Formatting identifier code) = "<" ++ token ++ ">" ++ toHtml code ++ "</" ++ token ++ ">" where token = formats M.! identifier
toHtml c@(Environment _ _) = environmentToHtml c
toHtml (More []) = ""
toHtml (More (h:hs)) = toHtml h ++ (toHtml (More hs))
toHtml (Inner str) = str

replaceInString :: String -> String -> String -> String
replaceInString key value input = case runParser (fullReplaceByP key value) input of
        Nothing -> "Replacing " ++ key ++ " by " ++ value ++ " in " ++ input ++ " went horribly wrong!"
        Just (_, replaced) -> replaced


parseBlogFile :: String -> IO ()
parseBlogFile path = do
    input <- readFile $ path ++ ".blog"
    html <- case preprocess input of 
        Nothing -> return "Preprocessing went horribly wrong!"
        Just (preprocessed, meta) -> do
            template <- readFile "HtmlTemplate.txt"
            let blogCode = runParser blogP preprocessed
                blogHtml = case blogCode of
                    Nothing -> "Parsing the blog went horribly wrong!"
                    Just (nothing, code) -> case null nothing of 
                        False -> toHtml code ++ "The blog did not fully parse!" 
                        True -> toHtml code                
                allInfo = M.insert "content" blogHtml meta
                almostThere = foldl (\sofar key -> replaceInString ("<!--%%%" ++ (map toUpper key) ++ "%%%-->") (if key `M.member` allInfo then allInfo M.! key else "") sofar) template $ delete "image" fullData
            if "image" `M.member` meta
                then return $ replaceInString "<!--%%%IMAGE%%%-->" ("<img src=\"../resources/pictures/" ++ (meta M.! "image") ++ "\" style=\"width: 100%;\">") almostThere 
                else return almostThere
    writeFile ("..\\blog\\" ++ takeBaseName path ++ ".html") html
                
            
            
