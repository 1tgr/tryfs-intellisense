namespace Tim.TryFSharp.Intellisense.Server

open System
open System.IO
open Nancy
open Nancy.Conventions
open Nancy.Diagnostics
open FSharp.InteractiveAutocomplete
open Microsoft.FSharp.Compiler.SourceCodeServices

type Ref<'Resource> =
    {
        Url : string<'Resource>
        Id : string
    }

type Error =
    {
        StartLine : int
        EndLine : int
        StartColumn : int
        EndColumn : int
        Severity : Severity
        Message : string
        Subcategory : string
    }

type Token =
    {
        Todo : unit
    }

[<Url("/{DraftId}/cell/{CellId}/errors")>]
type Errors = { DraftId : string; CellId : string } with
    interface IRestGet<Error array>

[<Url("/{DraftId}/cell/{CellId}/tokens")>]
type Tokens = { DraftId : string; CellId : string } with
    interface IRestGet<Token array>

[<Url("/{DraftId}/cell/{CellId}/completions{?Line,Column}")>]
type Completions = { DraftId : string; CellId : string; Line : int option; Column : int option } with
    interface IRestGet<string array>

[<Url("/{DraftId}/cell/{CellId}/code")>]
type Code = { DraftId : string; CellId : string } with
    interface IRestGet<string>
    interface IRestPut<string, unit>

type CellEntry =
    {
        Errors : string<Errors>
        Tokens : string<Tokens>
        Completions : string<Completions>
        Code : string<Code>
    }
    
[<Url("/{DraftId}/cell/{CellId}")>]
type Cell = { DraftId : string; CellId : string } with
    interface IRestGet<CellEntry>

type DraftEntry =
    {
        Cells : Ref<Cell> array
    }

[<Url("/{DraftId}")>]
type Draft = { DraftId : string } with
    interface IRestGet<DraftEntry>
    interface IRestDelete

[<Url("/")>]
type Drafts = | Drafts with
    interface IRestGet<Ref<Draft> array>
    interface IRestPost<unit, Ref<Draft>>

type IDraftContainer =
    abstract GetAgent : id : string -> IntelliSenseAgent * RequestOptions
    abstract WithAgent : id : string -> fn : (IntelliSenseAgent -> RequestOptions -> 'a * RequestOptions) -> 'a * RequestOptions

type DraftModule(drafts : IDraftContainer) as t =
    inherit NancyModule("/draft")

    let pos line column =
        match line, column with
        | Some line, Some column -> Some (line, column)
        | None, None -> None
        | None, _ -> failwithf "Please supply the 'line' query parameter"
        | _, None -> failwithf "Please supply the 'column' query parameter"

    let lineAt s n =
        use reader = new StringReader(s)
        
        let rec go lineStr i =
            if lineStr = null then
                ""
            elif i >= n then
                lineStr
            else
                go (reader.ReadLine()) (i + 1)

        go (reader.ReadLine()) 0

    let doCompletions ((line, _) as pos) (agent : IntelliSenseAgent) (opts : RequestOptions) =
        agent.DoCompletion(opts, pos, lineAt opts.Source line, None)

    do t.GetT <| fun Drafts ->
        [|
            { Url = t.UrlFor { DraftId = "a" }; Id = "a" }
            { Url = t.UrlFor { DraftId = "b" }; Id = "b" }
            { Url = t.UrlFor { DraftId = "c" }; Id = "c" }
        |]

    do t.GetT <| fun (draft : Draft) ->
        {
            Cells = [| { Url = t.UrlFor { DraftId = draft.DraftId; CellId = "1" }; Id = "1" } |]
        }

    do t.DeleteT <| fun (draft : Draft) ->
        ()

    do t.GetT <| fun (cell : Cell) ->
        {
            Errors =      t.UrlFor { DraftId = cell.DraftId; CellId = cell.CellId }
            Tokens =      t.UrlFor { DraftId = cell.DraftId; CellId = cell.CellId }
            Completions = t.UrlFor { DraftId = cell.DraftId; CellId = cell.CellId; Line = None; Column = None }
            Code =        t.UrlFor { DraftId = cell.DraftId; CellId = cell.CellId }
        }

    do t.GetT <| fun (errors : Errors) ->
        let agent, _ = drafts.GetAgent(errors.DraftId)
        [|
            for e in agent.GetErrors() ->
                {
                    StartLine = e.StartLine
                    EndLine = e.EndLine
                    StartColumn = e.StartColumn
                    EndColumn = e.EndColumn
                    Severity = e.Severity
                    Message = e.Message
                    Subcategory = e.Subcategory
                }
        |]

    do t.GetT <| fun (tokens : Tokens) ->
        [| |]

    do t.GetT <| fun (completions : Completions) ->
        let pos = defaultArg (pos completions.Line completions.Column) (0, 0)
        drafts.GetAgent(completions.DraftId) ||> doCompletions pos

    do t.GetT <| fun (code : Code) ->
        let _, opts = drafts.GetAgent(code.DraftId)
        opts.Source

    do t.PutT <| fun (code : Code) (text : string) ->
        let agent, opts =
            drafts.WithAgent code.DraftId <| fun agent _ ->
                agent, agent.CreateScriptOptions(Path.Combine(AppDomain.CurrentDomain.BaseDirectory, "script.fsx"), text)

        agent.TriggerParseRequest(opts, full = false)

type DraftContainer() =
    let syncRoot = obj()
    let mutable agents = Map.empty

    interface IDraftContainer with
        member t.WithAgent id fn =
            lock syncRoot <| fun () ->
                let agent, opts =
                    match Map.tryFind id agents with
                    | Some t -> t
                    | None ->
                        let agent = IntelliSenseAgent()
                        agent, agent.CreateScriptOptions(Path.Combine(AppDomain.CurrentDomain.BaseDirectory, "script.fsx"), "")

                let result, opts = fn agent opts
                agents <- Map.add id (agent, opts) agents
                result, opts

        member t.GetAgent(id) =
            (t :> IDraftContainer).WithAgent id <| fun agent opts -> agent, opts

type Bootstrapper() =
    inherit DefaultNancyBootstrapper()
    
    override t.ConfigureApplicationContainer(c) =
        base.ConfigureApplicationContainer(c)
        ignore (c.Register<IDraftContainer, DraftContainer>().AsSingleton())

    override t.ConfigureConventions(conventions) =
        base.ConfigureConventions(conventions)
        conventions.StaticContentsConventions.Add(StaticContentConventionBuilder.AddFile("/index.html", "index.html"))

    override t.DiagnosticsConfiguration =
        DiagnosticsConfiguration(Password = "admin")
