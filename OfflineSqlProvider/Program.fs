#if !INTERACTIVE
module Program
#else
#I @"packages\FParsec.1.0.2\lib\net40-client\"
#r @"FParsecCS.dll"
#r @"FParsec.dll"
#load "sqlSchema.fs"
#endif

open System
open FParsec
open sqlSchema
    
let test p str =
    match run p str with
    | Success(result, _, _)   -> printfn "Success: %A" result
    | Failure(errorMsg, _, _) -> printfn "Failure: %s" errorMsg

test tableDef "  CREATE TABLE dbo.whatever"

test fileOfComments """

/*
-- Whatever
      /*
      sd fsdf sd 
      */
*/

"""

[<EntryPoint>]
let main argv = 
    printfn "%A" argv
    0