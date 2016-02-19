module sqlTokenizer

open System
open FParsec

type UserState = unit
type Parser<'t> = Parser<'t, UserState>

type TextRange = { text : string ; posBegin : Position ; posEnd : Position }
type Token = Whitespace of TextRange | LineComment of TextRange | BlockComment of TextRange | Token of TextRange

let comment : Parser<_> = 
    let lineComment = 
        tuple3
            getPosition
            (pstring "--" .>>. restOfLine true |>> (fun (a, b) -> a + b))
            getPosition
        |>> (fun ( posBegin, comment, posEnd ) -> LineComment { text = comment ; posBegin = posBegin ; posEnd = posEnd })
    let blockComment =
        let (recComment : Parser<_>), commentRef = createParserForwardedToRef ()
        let commentChar : Parser<_> = 
            notFollowedBy (pstring "*/") >>. anyChar |>> string
        let commentContent : Parser<_> =
            many (choice [ recComment; commentChar ])
            |>> (fun cs -> String.Concat(cs))
        do commentRef := 
            tuple3 (pstring "/*") commentContent (pstring "*/")
            |>> (fun (a,b,c) -> a + b + c)
        let topComment =
            tuple3 getPosition recComment getPosition
            |>> (fun (posBegin, text, posEnd) -> BlockComment { text = text ; posBegin = posBegin ; posEnd = posEnd })
        topComment
    (lineComment <|> blockComment)

let ws : Parser<_> =
    let spacesKeep = many1Chars (satisfy Text.IsWhitespace)
    tuple3 getPosition spacesKeep getPosition
    |>> (fun (posBegin, text, posEnd) -> Whitespace { text = text ; posBegin = posBegin ; posEnd = posEnd })

let wsOrComments = ws <|> comment

let quotedIdentifier : Parser<_> =
    let textExceptUnescapedTerminator terminator = many1 (notFollowedBy (pstring terminator) >>. anyChar) |>> String.Concat
    tuple3
        getPosition
        (choice [
            tuple3 
                (pstring "[")
                (textExceptUnescapedTerminator "]")
                (pstring "]")
            tuple3 
                (pstring "'") 
                (textExceptUnescapedTerminator "'")
                (pstring "'")
            tuple3 
                (pstring "\"")
                (textExceptUnescapedTerminator "\"")
                (pstring "\"") ] 
            |>> (fun (a, b, c) -> a + b + c))
        getPosition
    |>> (fun (posBegin, text, posEnd) -> Token { text = text ; posBegin = posBegin ; posEnd = posEnd })

let operator : Parser<_> =
    tuple3
        getPosition
        (choice [
            pstring "+" ; pstring "-" ; pstring "*" ; pstring "/" ; pstring "%" ; pstring "~"
            pstring "&" ; pstring "|" ; pstring "^"
            pstring "=" ; pstring ">" ; pstring "<" ; pstring ">=" ; pstring "<=" ; pstring "<>" ; pstring "!=" ; pstring "!<" ; pstring "!>"
            pstring "+=" ; pstring "-=" ; pstring "*=" ; pstring "/=" ; pstring "%=" ; pstring "&=" ; pstring "^=" ; pstring "|="
            pstring "::" ; pstring ";" ])
        getPosition
    |>> (fun (posBegin, text, posEnd) -> Token { text = text ; posBegin = posBegin ; posEnd = posEnd })

let token = 
    tuple3
        getPosition
        (many1 (notFollowedBy (ws <|> comment <|> quotedIdentifier <|> operator) >>. anyChar) |>> String.Concat)
        getPosition
    |>> (fun (posBegin, text, posEnd) -> Token { text = text ; posBegin = posBegin ; posEnd = posEnd })
    
let sqlCodeListingTokenizer = many1 (choice [ ws ; comment ; quotedIdentifier ; operator ; token ]) .>> eof