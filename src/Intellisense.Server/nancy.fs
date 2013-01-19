namespace Tim.TryFSharp.Intellisense.Server

open System
open Microsoft.FSharp.Reflection
open Nancy

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

[<AttributeUsage(AttributeTargets.Class, AllowMultiple = true)>]
type UrlAttribute(template  : string) =
    inherit Attribute()

    member t.Template = template

[<AutoOpen>]
module NancyModuleExtensions =

    [<Sealed>]
    type private RestResourceImpl<'Resource> private () =
        static let typ = typeof<'Resource>

        static let urls =
            [|
                for (attr : UrlAttribute) in Seq.cast (typ.GetCustomAttributes(typeof<UrlAttribute>, true)) ->
                    attr.Template
            |]

        static let create : (string -> obj) -> 'Resource =
            let fields = FSharpType.GetRecordFields(typ) |> Array.map (fun pi -> pi.Name)
            let ctor = FSharpValue.PreComputeRecordConstructor(typ)
            fun fieldFn ->
                let values = Array.map fieldFn fields
                unbox (ctor values)

        static member Urls = urls
        static member Create = create

    let private dynamicDictionaryField (dict : obj) (name : string) : obj =
        let dict : DynamicDictionary = unbox dict
        let value : DynamicDictionaryValue = unbox (dict.[name])
        value.Value

    type NancyModule with
        member t.GetT<'Resource, 'Response when 'Resource :> IRestGet<'Response>> (fn : 'Resource -> 'Response) =
            for url in RestResourceImpl<'Resource>.Urls do
                printfn "Registering route %s for %O" url typeof<'Resource>
                t.Get.[url] <- fun parameters ->
                    let resource = RestResourceImpl<'Resource>.Create (dynamicDictionaryField parameters)
                    box (fn resource)
