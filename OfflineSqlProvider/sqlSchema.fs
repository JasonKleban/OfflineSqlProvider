module sqlSchema

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

let fileOfComments = many1 wsOrComments .>> eof

let quotedIdentifier : Parser<_> =
    let textExceptUnescapedTerminator terminator = (pchar terminator >>. pchar terminator) <|> anyChar
    choice [
        (pchar '[' >>. textExceptUnescapedTerminator ']' .>> pchar ']')
        (pchar '"' >>. textExceptUnescapedTerminator '"' .>> pchar '"')
        (pchar '\'' >>. textExceptUnescapedTerminator '\'' .>> pchar '\'') ] 
    |>> string

//let unquotedIdentifier : Parser<_> = 
//    let isAsciiIdStart    = fun c -> isAsciiLetter c || c = '_'
//    let isAsciiIdContinue = fun c -> isAsciiLetter c || isDigit c || c = '_' || c = '$' || c = '@'
//    identifier (IdentifierOptions(
//                    isAsciiIdStart = isAsciiIdStart,
//                    isAsciiIdContinue = isAsciiIdContinue,
//                    normalization = System.Text.NormalizationForm.FormKC,
//                    normalizeBeforeValidation = true,
//                    allowAllNonAsciiCharsInPreCheck = true))

let identifier = quotedIdentifier // <|> unquotedIdentifier

let tableDef : Parser<_> = 
    spaces 
    >>. pstringCI "CREATE" 
    >>. spaces 
    >>. pstringCI "TABLE"
    >>. spaces