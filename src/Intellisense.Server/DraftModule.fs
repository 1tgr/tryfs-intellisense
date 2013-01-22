namespace Tim.TryFSharp.Intellisense.Server

open Nancy
open FSharp.InteractiveAutocomplete

type Ref<'Resource> =
    {
        Url : string<'Resource>
        Id : string
    }

type Token =
    {
        Todo : unit
    }

type ParseResult =
    {
        Tokens : Token array
        Completions : string array
    }

[<Url("/{DraftId}/cell/{CellId}/tokens")>]
type Tokens = { DraftId : string; CellId : string } with
    interface IRestGet<Token array>

[<Url("/{DraftId}/cell/{CellId}/completions{?Line,Column}")>]
type Completions = { DraftId : string; CellId : string; Line : int option; Column : int option } with
    interface IRestGet<string array>

[<Url("/{DraftId}/cell/{CellId}/code{?Line,Column}")>]
type Code = { DraftId : string; CellId : string; Line : int option; Column : int option } with
    interface IRestGet<string>
    interface IRestPut<string, ParseResult>

type CellEntry =
    {
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
            Tokens =      t.UrlFor { DraftId = cell.DraftId; CellId = cell.CellId }
            Completions = t.UrlFor { DraftId = cell.DraftId; CellId = cell.CellId; Line = None; Column = None }
            Code =        t.UrlFor { DraftId = cell.DraftId; CellId = cell.CellId; Line = None; Column = None }
        }

    do t.GetT <| fun (tokens : Tokens) ->
        [| |]

    do t.GetT <| fun (completions : Completions) ->
        let pos = defaultArg (pos completions.Line completions.Column) (0, 0)
        let agent, opts = drafts.GetAgent(completions.DraftId)
        agent.DoCompletion(opts, pos, "", None)

    do t.GetT <| fun (code : Code) ->
        let _, opts = drafts.GetAgent(code.DraftId)
        opts.Source

    do t.PutT <| fun (code : Code) (text : string) ->
        let agent, opts =
            drafts.WithAgent code.DraftId <| fun agent _ ->
                agent, agent.CreateScriptOptions("script.fsx", text)

        agent.TriggerParseRequest(opts, full = false)
        
        let completions =
            match pos code.Line code.Column with
            | Some pos -> agent.DoCompletion(opts, pos, "", None)
            | None -> [| |]

        { Tokens = [| |]; Completions = completions }

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
                        agent, agent.CreateScriptOptions("script.fsx", "")

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
