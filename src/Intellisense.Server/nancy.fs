#nowarn "42"
namespace Tim.TryFSharp.Intellisense.Server

open System
open Microsoft.FSharp.Reflection
open Nancy
open Nancy.ModelBinding

type IRestResource =
    interface end

type IRestGet<'Response> =
    inherit IRestResource

type IRestPut<'Request, 'Response> =
    inherit IRestResource

type IRestPost<'Request, 'Response> =
    inherit IRestResource

type IRestDelete =
    inherit IRestResource

[<MeasureAnnotatedAbbreviation>]
type string<'Measure> = string

[<AttributeUsage(AttributeTargets.Class, AllowMultiple = true)>]
type UrlAttribute(template  : string) =
    inherit Attribute()

    member t.Template = template

[<AutoOpen>]
module NancyModuleExtensions =

    [<Sealed>]
    type private RestResourceImpl<'Resource> private () =
        static let typ = typeof<'Resource>
        static let fieldNames = FSharpType.GetRecordFields(typ) |> Array.map (fun pi -> pi.Name)
        static let ctor = FSharpValue.PreComputeRecordConstructor(typ)
        static let reader = FSharpValue.PreComputeRecordReader(typ)

        static let urls =
            [|
                for (attr : UrlAttribute) in Seq.cast (typ.GetCustomAttributes(typeof<UrlAttribute>, true)) ->
                    attr.Template
            |]

        static member Urls = urls

        static member PartsToResource (part : string -> obj) : 'Resource =
            let values = Array.map part fieldNames
            unbox (ctor values)

        static member ResourceToString (resource : 'Resource) : string =
            match urls with
            | [| |] -> failwithf "No [<Url>] attribute on %O" typeof<'Resource>
            | a ->
                let values = reader resource

                String.concat "/" <| seq {
                    for part in a.[0].Split('/') ->
                        if part.StartsWith("<") && part.EndsWith(">") then
                            let part = part.Substring(1, part.Length - 2)
                            match Array.IndexOf(fieldNames, part) with
                            | n when n >= 0 -> string (values.[n])
                            | _ -> failwithf "No field called '%s' in %O" part typeof<'Resource>

                        else
                            part
                }


    let private dynamicDictionaryField (dict : obj) (name : string) : obj =
        let dict : DynamicDictionary = unbox dict
        let value : DynamicDictionaryValue = unbox (dict.[name])
        value.Value

    let private handler (fn : 'a -> 'b) : obj -> obj =
        dynamicDictionaryField >> RestResourceImpl<_>.PartsToResource >> fn >> box

    [<NoDynamicInvocation>]
    let inline private retype (x : 'a) : 'b = (# "" x : 'b #)

    type NancyModule with
        member t.UrlFor (resource : 'Resource) : string<'Resource> =
            retype (RestResourceImpl<_>.ResourceToString(resource))

        member t.GetT<'Resource, 'Response when 'Resource :> IRestGet<'Response>> (fn : 'Resource -> 'Response) =
            for url in RestResourceImpl<'Resource>.Urls do
                t.Get.[url] <- Func<_,_>(handler fn)

        member t.PutT<'Resource, 'Request, 'Response when 'Resource :> IRestPut<'Request, 'Response>> (fn : 'Request -> 'Resource -> 'Response) =
            for url in RestResourceImpl<'Resource>.Urls do
                t.Put.[url] <- fun parameters ->
                    let request = t.Bind<_>()
                    handler (fn request) parameters

        member t.PostT<'Resource, 'Request, 'Response when 'Resource :> IRestPost<'Request, 'Response>> (fn : 'Request -> 'Resource -> 'Response) =
            for url in RestResourceImpl<'Resource>.Urls do
                t.Post.[url] <- fun parameters ->
                    let request = t.Bind<_>()
                    handler (fn request) parameters

        member t.DeleteT<'Resource when 'Resource :> IRestDelete> (fn : 'Resource -> unit) =
            for url in RestResourceImpl<'Resource>.Urls do
                t.Delete.[url] <- Func<_,_>(handler fn)


