module sqlTokenizer

open System
open FParsec

type UserState = unit
type Parser<'t> = Parser<'t, UserState>

type TextRange = { text : string ; posBegin : Position ; posEnd : Position }

type Token =
    | Whitespace of TextRange
    | LineComment of TextRange
    | BlockComment of TextRange
    | Token of TextRange

let comment : Parser<_> = 
    let lineComment = 
        getPosition
        .>>. (pstring "--" .>>. restOfLine true)
        .>>. getPosition
        |>> (fun ((posBegin, (commentStart, comment)), posEnd) -> LineComment { text = commentStart + comment ; posBegin = posBegin ; posEnd = posEnd })
    let blockComment =
        /// Captures C-style multiline comment /*...*/ with arbitrary nesting depth.
        let (recComment : Parser<_>), commentRef = createParserForwardedToRef ()
        /// Captures any character that is not the beginning of a comment end marker */.
        let commentChar : Parser<_> = 
            notFollowedBy (pstring "*/") 
            >>. anyChar 
            |>> string
        /// Skips any mix of nested comments or comment characters.
        let commentContent : Parser<_> =
            many (choice [ recComment; commentChar ])
            |>> (fun cs -> String.Concat(cs))
        // Skips C-style multiline comment /*...*/ with arbitrary nesting depth.
        do commentRef := 
            (pstring "/*")
            .>>. commentContent
            .>>. (pstring "*/")
            |>> (fun ((a,b),c) -> a + b + c)
        let topComment =
            getPosition
            .>>. recComment
            .>>. getPosition
            |>> (fun ((posBegin, text), posEnd) -> BlockComment { text = text ; posBegin = posBegin ; posEnd = posEnd })
        topComment
    (lineComment <|> blockComment)

let ws : Parser<_> =
    let spacesKeep =
        many1Chars (satisfy Text.IsWhitespace)
    getPosition
    .>>. spacesKeep
    .>>. getPosition
    |>> (fun ((posBegin, text), posEnd) -> Whitespace { text = text ; posBegin = posBegin ; posEnd = posEnd })

let wsOrComments = ws <|> comment

let quotedIdentifier : Parser<_> =
    let textExceptUnescapedTerminator terminator = many1 (notFollowedBy (pstring terminator) >>. anyChar) |>> String.Concat
    getPosition
    .>>. (choice [
            (pstring "[" .>>. textExceptUnescapedTerminator "]" .>>. pstring "]")
            (pstring "'" .>>. textExceptUnescapedTerminator "'" .>>. pstring "'")
            (pstring "\"" .>>. textExceptUnescapedTerminator "\"" .>>. pstring "\"") ] 
            |>> (fun ((a, b), c) -> a + b + c))
    .>>. getPosition
    |>> (fun ((posBegin, text), posEnd) -> Token { text = text ; posBegin = posBegin ; posEnd = posEnd })

let operator : Parser<_> =
    getPosition
    .>>. choice [
            pstring "+" ; pstring "-" ; pstring "*" ; pstring "/" ; pstring "%" ; pstring "~"
            pstring "&" ; pstring "|" ; pstring "^"
            pstring "=" ; pstring ">" ; pstring "<" ; pstring ">=" ; pstring "<=" ; pstring "<>" ; pstring "!=" ; pstring "!<" ; pstring "!>"
            pstring "+=" ; pstring "-=" ; pstring "*=" ; pstring "/=" ; pstring "%=" ; pstring "&=" ; pstring "^=" ; pstring "|="
            pstring "::" ; pstring ";"
        ]
    .>>. getPosition
    |>> (fun ((posBegin, text), posEnd) -> Token { text = text ; posBegin = posBegin ; posEnd = posEnd })

let token = 
    getPosition
    .>>. (many1 (notFollowedBy (ws <|> comment <|> quotedIdentifier <|> operator) >>. anyChar) |>> String.Concat)
    .>>. getPosition
    |>> (fun ((posBegin, text), posEnd) -> Token { text = text ; posBegin = posBegin ; posEnd = posEnd })
    
let fileOfComments = many1 (choice [ ws ; comment ; quotedIdentifier ; operator ; token ]) .>> eof

let identifier = quotedIdentifier