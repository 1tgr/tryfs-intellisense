namespace Tim.TryFSharp.Intellisense.Server

open Nancy

[<Url("/{id}")>]
type Draft = { Id : string } with
    interface IRestDelete

type Ref<'Resource> =
    {
        Url : string<'Resource>
        Id : string
    }

type Drafts = | Drafts with
    interface IRestGet<Ref<Draft> array>
    interface IRestPost<unit, Ref<Draft>>

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

type HelloModule() as t =
   inherit NancyModule("/draft")

   (*
   do t.GetT <| fun Drafts ->
       [|
           { Url = t.UrlFor { Id = "a" }; Id = "a" }
           { Url = t.UrlFor { Id = "b" }; Id = "b" }
           { Url = t.UrlFor { Id = "c" }; Id = "c" }
       |]
   *)

   do t.DeleteT <| fun (draft : Draft) ->
       ()

   do t.GetT <| fun (tokens : Tokens) ->
       [| |]

   do t.GetT <| fun (completions : Completions) ->
       [| |]

   do t.GetT <| fun (code : Code) ->
       ""

   do t.PutT <| fun (text : string) (code : Code) ->
       { Tokens = [| |]; Completions = [| |] }
