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

[<Url("/{DraftId}/cell/{CellId}/completions")>]
type Completions = { DraftId : string; CellId : string } with
    interface IRestGet<string array>

[<Url("/{DraftId}/cell/{CellId}/code")>]
type Code = { DraftId : string; CellId : string } with
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
    abstract GetAgent : id : string -> IntelliSenseAgent

type DraftModule(drafts : IDraftContainer) as t =
    inherit NancyModule("/draft")

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
            Completions = t.UrlFor { DraftId = cell.DraftId; CellId = cell.CellId }
            Code =        t.UrlFor { DraftId = cell.DraftId; CellId = cell.CellId }
        }

    do t.GetT <| fun (tokens : Tokens) ->
        [| |]

    do t.GetT <| fun (completions : Completions) ->
        [| |]

    do t.GetT <| fun (code : Code) ->
        ""

    do t.PutT <| fun (code : Code) (text : string) ->
        { Tokens = [| |]; Completions = [| |] }

type DraftContainer() =
    let syncRoot = obj()
    let mutable agents = Map.empty

    interface IDraftContainer with
        member t.GetAgent(id) =
            lock syncRoot <| fun () ->
                match Map.tryFind id agents with
                | Some agent -> agent
                | None ->
                    let agent = IntelliSenseAgent()
                    agents <- Map.add id agent agents
                    agent

type Bootstrapper() =
    inherit DefaultNancyBootstrapper()
    
    override t.ConfigureApplicationContainer(c) =
        base.ConfigureApplicationContainer(c)
        ignore (c.Register<IDraftContainer, DraftContainer>().AsSingleton())
