#if !INTERACTIVE
module Program
#else
#I @"packages\FParsec.1.0.2\lib\net40-client\"
#r @"FParsecCS.dll"
#r @"FParsec.dll"
#load "tokenizer.fs"
#load "sqlDdl.fs"
#endif

open System
open FParsec
open sqlTokenizer
open sqlDdl
    
let test p str =
    match run p str with
    | Success(result, _, _)   -> printfn "Success: %A" result
    | Failure(errorMsg, _, _) -> printfn "Failure: %s" errorMsg

test sqlCodeListingTokenizer """

SELECT 0; GO

/*
-- Whatever
      /*
      sd fsdf sd 
      */
*/

CREATE TABLE [dbo].[whatever i want] (
    ID INT IDENTITY CONSTRAINT PK_whatever PRIMARY KEY
)

"""