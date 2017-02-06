> module Main where
> import Network.HTTP.Base
> import Debug.Trace

This is a literate Haskell marker upper

> import Text.ParserCombinators.Parsec

> eol = newline <|> (eof >> return '\n')
> tilEOL = manyTill (try esc <|> (noneOf "\n" >>= \x -> return [x])) eol

> codeLine :: GenParser Char st [Char]
> codeLine = do
>     string "> "
>     code <- tilEOL
>     return $ "&gt; " ++ concat code

> nonLhsLine :: GenParser Char st [Char]
> nonLhsLine = do
>     string "< "
>     code <- tilEOL
>     return $ concat code

> character = do 
>   c <- noneOf "$\n"
>   return [c]

> notBrace = do 
>   c <- noneOf "$\n}"
>   return [c]

> notLine = do 
>   c <- noneOf "$\n|"
>   return [c]

> bq = do
>   try (string "\\[")
>   return "<blockquote>"

> nbq = do
>   try (string "\\]")
>   return "</blockquote>"

> command = do
>   try (char '\\')
>   name <- many1 letter
>   let value = case lookup name [
>                      ("back","\\"),
>                      ("dollar","$"),
>                      ("exist","&exist;"),
>                      ("forall","&forall;"),
>                      ("dollar","$"),
>                      ("or","&or;"),
>                      ("and","&and;"),
>                      ("not","&not;"),
>                      ("lbrace","{"),
>                      ("rbrace","}"),
>                      ("times","&times;"),
>                      ("otimes","&otimes;"),
>                      ("epsilon","&epsilon;"),
>                      ("delta","&delta;"),
>                      ("Delta","&Delta;"),
>                      ("eta","&eta;"),
>                      ("mu","&mu;"),
>                      ("cdot","&middot;"),
>                      ("empty","&empty;"),
>                      ("in","&isin;"),
>                      ("ni","&ni;"),
>                      ("sigma","&sigma;"),
>                      ("Sigma","&Sigma;"),
>                      ("infty","&infin;"),
>                      ("omega","&omega;"),
>                      ("lambda","&lambda;"),
>                      ("pi","&pi;"),
>                      ("le","&le;"),
>                      ("ge","&ge;"),
>                      ("sub","&sub;"),
>                      ("cup","&cup;"),
>                      ("cap","&cap;"),
>                      ("sqrt","&radic;"),
>                      ("rightarrow","&rarr;"),
>                      ("leftrightarrow","&harr;"),
>                      ("subset","&sub;"),
>                      ("R","&#x211d;"),
>                      ("Q","&#x211a;"),
>                      ("half","&#xbd;"),
>                      ("sum","&Sigma;"),
>                      ("o","&ouml;"),
>                      ("box","&#x25fb;"),
>                      ("dia","&#x25ca;"),
>                      ("top","&#x22a4;"),
>                      ("bot","&perp;"),
>                      ("proarrow","&#x219b;"),
>                      ("circ","&#x25cb;"),
>                      ("implies","&rArr;")
>                   ] of 
>                   Just x  -> x
>                   Nothing -> "*XXX ERROR XXX*";
>   many (char ' ')
>   return value

> title = do
>   try (string "\\section{")
>   ms <- many1 braceUnit
>   string "}"
>   return $ "<BR><b>" ++ concat ms ++ "</b><p>"

> bold = do
>   try (string "\\bf")
>   ms <- block
>   return $ "<b>" ++ ms ++ "</b>"

> italic = do
>   try (string "\\it")
>   ms <- block
>   return $ "<i>" ++ ms ++ "</i>"

> block = do
>   try (char '{')
>   ms <- many1 braceUnit
>   char '}'
>   return $ concat ms

> subscript = do
>   char '_'
>   cs <- block
>   return $ "<sub>" ++ cs ++ "</sub>"

> overline = do
>   try (string "\\bar")
>   cs <- block
>   return $ "<span style=\"text-decoration: overline\">" ++ cs ++ "</span>"

> superscript = do
>   char '^'
>   cs <- block
>   return $ "<sup>" ++ cs ++ "</sup>"

> esc = (char '<' >> return "&lt;")
>   <|> (char '>' >> return "&gt;")
>   <|> (char '&' >> return "&amp;")

> unit = lit <|> verbatim <|> enumerate <|> try bold <|> try overline <|> try italic <|> title <|> try esc <|> bq <|> nbq <|> block <|> command <|> codeExpr <|> try mathExpr <|> try subscript <|> try superscript <|> try link <|> character 

> braceUnit = title <|> try bold <|> try overline <|> try italic <|> try esc <|> bq <|> nbq <|> block <|> command <|> codeExpr <|> try mathExpr <|> try subscript <|> try superscript <|> try link <|> notBrace

> codeUnit = title <|> try bold <|> try overline <|>  try italic <|> try esc <|> bq <|> nbq <|> block <|> command <|> codeExpr <|> try mathExpr <|> try subscript <|> try superscript <|> try link <|> notLine

> link = do
>   char '['
>   text <- many1 (noneOf "]")
>   string "]("
>   url <- many1 (noneOf ")")
>   char ')'
>   return $ "<a href=\"" ++ url ++ "\">" ++ text ++ "</a>"

> enumerateItem = do
>   string "\\item"
>   text <- block
>   char '\n'
>   return $ "<li>" ++ text ++ "</li>\n"

> enumerate = do
>   try (string "\\begin{enumerate}\n")
>   text <- many1 enumerateItem
>   char '!'
>   return $ "<ol>" ++ concat text ++ "</ol>"

> verbatim = do
>   try (string "\\begin{verbatim}")
>   text <- many1 (noneOf "!")
>   char '!'
>   return $ "<pre>" ++ text ++ "</pre>"

> lit = do
>   try (string "\\begin{lit}")
>   text <- many1 (noneOf "!")
>   char '!'
>   return $ text

> mathExpr = do
>   char '$'
>   ms <- many1 (noneOf "$")
>   char '$'
>   let url = "https://chart.googleapis.com/chart?cht=tx&chl=" ++ urlEncode ms
>   return $ "<img src=\"" ++ url ++ "\" style=\"vertical-align:middle\">"

> codeExpr = do
>   try (string "-|")
>   ms <- many1 codeUnit
>   string "|-"
>   return $ "<tt>" ++ concat ms ++ "</tt>"

> textLine :: GenParser Char st [Char]
> textLine = do
>   c <- unit
>   text <- many unit
>   eol
>   return $ c ++ concat text

> blankLine = do
>   char '\n'
>   return "<P><BR>"

Here is some text.
Over two lines with some $x+1$.

> literateBlock = do
>   c <- codeLine
>   cs <- many1 (try codeLine <|> blankLine)
>   return $ "<pre>\n" ++ concat (map (++ "\n") (c:cs)) ++ "</pre>"

> nonLhsBlock = do
>   c <- nonLhsLine
>   cs <- many1 (try nonLhsLine <|> blankLine)
>   return $ "<pre>\n" ++ concat (map (++ "\n") (c:cs)) ++ "</pre>"

> textBlock = do
>   ls <- many1 (try textLine)
>   return $ concat (map (++ "\n") ls)

> lhs = many1 (try literateBlock <|> try nonLhsBlock <|> try blankLine <|> textLine)

> main = do
>   s <- getContents
>   case parse lhs "stdin" s of
>       Left err -> print err
>       Right cs -> mapM_ putStrLn cs
