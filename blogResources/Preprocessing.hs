module Preprocessing where 

import BasicParsing
import qualified Data.Map as M
import Control.Applicative

--typedef
type NewCommands = M.Map String String


--handling new commands

newCommandP :: Parser (String, String)
newCommandP = (\key value -> ('\\':key, value)) <$> (stringP "\\newcommand{\\"
                *> inBracesP <* (stringP "}{"))
                <*> (inBracesP <* (charP '}') <* ignoreLinebreakP)

 

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



replaceCommandsP :: NewCommands -> Parser String
replaceCommandsP commands = asum (M.mapWithKey replaceByP commands)

--extract metadata

metaData = ["title", "subtitle", "date", "image"]
fullData = ("content":"tldr":metaData)

extractMetaDataP = extractManyP metaData

eraseMetaDataP :: Parser String
eraseMetaDataP = untouchedP $ asum $ map extractAndRememberKeyP metaData

extractTldrP :: Parser String
extractTldrP = concat <$> many (extractFromEnvironmentP "tldr" <|> consumeCharP)

--full preprocess: collect + implement macros; extract + erase metadata  

preprocess :: String -> Maybe (String, NewCommands)
preprocess input = do
      (_, (commands, leftover)) <- runParser collectNewCommandsP input
      (nothing, implementedMacros) <- runParser (concat <$> (many $ replaceCommandsP commands <|> anyCharP)) leftover
      if not $ null nothing then Nothing 
        else do
          (_, meta) <- runParser extractMetaDataP implementedMacros
          (_, erasedMeta) <- runParser eraseMetaDataP implementedMacros
          (_, tldr) <- runParser extractTldrP erasedMeta
          (_, cleaned) <- runParser (untouchedP $ extractFromEnvironmentP "tldr") erasedMeta
          let fullMeta = M.insert "tldr" tldr meta
          return (cleaned, fullMeta)