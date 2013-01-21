// --------------------------------------------------------------------------------------
// (c) Tomas Petricek, http://tomasp.net/blog
// --------------------------------------------------------------------------------------
namespace FSharp.InteractiveAutocomplete

open System
open System.IO
open System.Collections.Generic

open Microsoft.FSharp.Compiler
open Microsoft.FSharp.Compiler.SourceCodeServices
open FSharp.InteractiveAutocomplete.Parsing
open FSharp.CompilerBinding.Reflection

module FsParser = Microsoft.FSharp.Compiler.Parser

// --------------------------------------------------------------------------------------
// IntelliSense agent - provides easier an access to F# IntelliSense service
// We're using a simple agent, because requests should be done from a single thread 
// --------------------------------------------------------------------------------------

/// Represents information needed to call the F# IntelliSense service
/// (including project/script options, file name and source)
type RequestOptions(opts, file, src) =
  member x.Options : CheckOptions = opts
  member x.FileName : string = file
  member x.Source : string = src
  member x.WithSource(source) = 
    RequestOptions(opts, file, source)


/// Message type that is used by 'IntelliSenseAgent'
type internal IntelliSenseAgentMessage = 
  | TriggerParseRequest of RequestOptions * bool
  | GetTypeCheckInfo of RequestOptions * int option * AsyncReplyChannel<TypeCheckInfo option>
  | GetErrors of AsyncReplyChannel<ErrorInfo[]>
  | GetDeclarationsMessage of RequestOptions * AsyncReplyChannel<TopLevelDeclaration[]>


/// Provides an easy access to F# IntelliSense service
type IntelliSenseAgent() =
  
  /// Create an F# IntelliSense service 
  let checker = InteractiveChecker.Create(ignore)

  /// Creates an empty "Identifier" token (we need it when getting ToolTip)
  let identToken = FsParser.tagOfToken(FsParser.token.IDENT("")) 

  /// Calls F# IntelliSense service repeatedly in an (asynchronous) loop
  /// until the type check request succeeds
  let rec waitForTypeCheck(opts:RequestOptions, untypedInfo) = async {
    let info = 
      checker.TypeCheckSource
        ( untypedInfo, opts.FileName, identToken, opts.Source, 
          opts.Options, IsResultObsolete(fun () -> false) ) 
    match info with
    | TypeCheckSucceeded(res) when res.TypeCheckInfo.IsSome ->
        return res.TypeCheckInfo.Value, res.Errors
    | _ -> 
        do! Async.Sleep(200)
        return! waitForTypeCheck(opts, untypedInfo) }

  /// Start the agent - the agent remembers some state
  /// (currently just a list of errors from the last parse)
  let agent = MailboxProcessor.Start(fun agent -> 
    let rec loop errors = async {
      let! msg = agent.Receive()
      match msg with 
      | TriggerParseRequest(opts, full) ->
          // Start parsing and update errors with the new ones
          let untypedInfo = checker.UntypedParse(opts.FileName, opts.Source, opts.Options) 
          let res = 
            checker.TypeCheckSource
              ( untypedInfo, opts.FileName, 0, opts.Source, 
                opts.Options, IsResultObsolete(fun () -> false)) 
          let errors = 
            match res with
            | TypeCheckSucceeded(res) -> res.Errors
            | _ -> errors
          // Start full background parsing if requested..
          if full then checker.StartBackgroundCompile(opts.Options) 
          return! loop errors 

      | GetDeclarationsMessage(opts, repl) ->
          let untypedInfo = checker.UntypedParse(opts.FileName, opts.Source, opts.Options) 
          repl.Reply(untypedInfo.GetNavigationItems().Declarations)
          return! loop errors 

      | GetTypeCheckInfo(opts, timeout, reply) ->
          // Try to get information for the IntelliSense (in the specified time)
          let untypedInfo = checker.UntypedParse(opts.FileName, opts.Source, opts.Options) 
          try
            let res, errors = 
              Async.RunSynchronously
                (waitForTypeCheck(opts, untypedInfo), ?timeout = timeout)
            reply.Reply(Some(res))
            return! loop errors
          with :? OperationCanceledException ->
            reply.Reply(None) 
            return! loop errors 

      | GetErrors(reply) ->
          // Return an array with errors that were reported last time
          reply.Reply(errors)
          return! loop errors }
    loop [||] )

  /// Returns default F# compiler options for the specified script file (FSX)
  member x.CreateScriptOptions(file, source) =
    RequestOptions
      ( checker.GetCheckOptionsFromScriptRoot(file, source, DateTime.Now),
        file, source )

  /// Get errors from the last parse request
  member x.GetErrors() = 
    agent.PostAndReply(GetErrors)

  /// Get declarations from the last parse request
  member x.GetDeclarations(opts) = 
    agent.PostAndReply(fun repl -> GetDeclarationsMessage(opts, repl))
      
  /// Trigger background parse request
  member x.TriggerParseRequest(opts, full) = 
    agent.Post(TriggerParseRequest(opts, full))

  /// Invokes dot-completion request and writes information to the standard output
  member x.DoCompletion(opts, ((line, column) as pos), lineStr, time) =
    try
      try
        // Get the long identifier before the current location
        // 'residue' is the part after the last dot and 'longName' is before
        // e.g.  System.Console.Wri  --> "Wri", [ "System"; "Console"; ]
        let lookBack = Parsing.createBackStringReader lineStr (column - 1)
        let residue, longName = 
          lookBack |> Parsing.getFirst Parsing.parseBackIdentWithResidue

        // Try to get type information & run the request
        let op = agent.PostAndAsyncReply(fun r -> GetTypeCheckInfo(opts, time, r))
        let info = Async.RunSynchronously(op, ?timeout = time)
        match info with 
        | Some(info) ->
            // Get items & generate output
            let decls = info.GetDeclarations(pos, lineStr, (longName, residue), 0, defaultArg time 1000)
            for d in decls.Items do Console.WriteLine(d.Name)
        | None -> ()
      with :? OperationCanceledException -> ()
    finally Console.WriteLine("<<EOF>>")


  /// Gets ToolTip for the specified location (and prints it to the output)
  member x.GetToolTip(opts, ((line, column) as pos), lineStr, time) =
    try
      try
        // Try to get type information & run the request
        let op = agent.PostAndAsyncReply(fun r -> GetTypeCheckInfo(opts, time, r))
        match Async.RunSynchronously(op, ?timeout = time) with 
        | None -> ()
        | Some(info) ->
            // Parsing - find the identifier around the current location
            // (we look for full identifier in the backward direction, but only
            // for a short identifier forward - this means that when you hover
            // 'B' in 'A.B.C', you will get intellisense for 'A.B' module)
            let lookBack = Parsing.createBackStringReader lineStr column
            let lookForw = Parsing.createForwardStringReader lineStr (column + 1)
            let backIdent = Parsing.getFirst Parsing.parseBackLongIdent lookBack
            let nextIdent = Parsing.getFirst Parsing.parseIdent lookForw
    
            let identIsland =
              match List.rev backIdent with
              | last::prev -> (last + nextIdent)::prev |> List.rev
              | [] -> []

            match identIsland with
            | [ "" ] -> 
                // There is no identifier at the current location
                ()
            | _ -> 
                // Assume that we are inside identifier (F# services can also handle
                // case when we're in a string in '#r "Foo.dll"' but we don't do that)
                let tip = info.GetDataTipText(pos, lineStr, identIsland, identToken)
                match tip with
                | DataTipText(elems) 
                    when elems |> List.forall (function 
                      DataTipElementNone -> true | _ -> false) -> ()
                | _ -> 
                    Console.WriteLine(TipFormatter.formatTip tip)
      with :? OperationCanceledException -> ()
    finally Console.WriteLine("<<EOF>>")
