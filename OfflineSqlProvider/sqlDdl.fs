module sqlDdl

open System
open FParsec
open sqlTokenizer


//
//let whitespace : Parser<_> =
//    satisfy (
//                function
//                | Token.Whitespace (_) -> true
//                | _ -> false )
//
//let sqlDDLCodeListing : Parser<_> = 
//    sqlCodeListingTokenizer
//    >>= (fun t -> 
//        many (satisfy Token.Whitespace))
