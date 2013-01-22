#nowarn "40"
#nowarn "42"
namespace Tim.TryFSharp.Intellisense.Server

open System
open System.Collections.Generic
open System.IO
open System.Text
open System.Web
open Microsoft.FSharp.Reflection
open Nancy
open Nancy.ModelBinding
open Tavis.UriTemplates

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

    let private memoize (fn : 'a -> 'b) : 'a -> 'b =
        let cache = Dictionary()
        let syncRoot = obj()
        fun a ->
            lock syncRoot <| fun () ->
                match cache.TryGetValue(a) with
                | true, b -> b
                | false, _ ->
                    let b = fn a
                    cache.[a] <- b
                    b

    let rec private convert : Type -> string -> obj =
        memoize <| fun toType ->
            if toType.IsGenericType && toType.GetGenericTypeDefinition() = typedefof<option<_>> then
                let someCase, convertInner =
                    FSharpType.GetUnionCases(toType)
                    |> Array.pick (fun c ->
                        match c.GetFields() with
                        | [| f |] -> Some (c, convert f.PropertyType)
                        | _ -> None)

                let ctor = FSharpValue.PreComputeUnionConstructor(someCase)
                function
                | null | "" -> null
                | s -> ctor [| convertInner s |]

            elif typeof<IConvertible>.IsAssignableFrom(toType) then
                fun s -> Convert.ChangeType(s, toType)

            else
                failwithf "Can't convert string to %O" toType
            

    [<Sealed>]
    type private RestResourceImpl<'Resource> private () =
        static let typ = typeof<'Resource>

        static let fields, ctor, reader =
            if FSharpType.IsUnion(typ) then
                match FSharpType.GetUnionCases(typ) with
                | [| c |] when c.GetFields() = [| |] ->
                    let value = FSharpValue.MakeUnion(c, [| |])
                    [| |], (fun _ -> value), (fun _ -> [| |])
                | a -> failwithf "Unions must only have one case. %O has %d cases." typ a.Length

            elif FSharpType.IsRecord(typ) then
                [| for pi in FSharpType.GetRecordFields(typ) -> pi.Name, convert pi.PropertyType |],
                    FSharpValue.PreComputeRecordConstructor(typ),
                    FSharpValue.PreComputeRecordReader(typ)

            else
                failwithf "Only single-case unions and records are supported, not %O" typ

        static let urls =
            match typ.GetCustomAttributes(typeof<UrlAttribute>, true) with
            | [| |] -> failwithf "No [<Url>] attribute on %O" typeof<'Resource>
            | a -> [| for attr in a -> (unbox<UrlAttribute> attr).Template |]

        static member Urls = urls

        static member PartsToResource (part : string -> string) : 'Resource =
            let values = Array.map (fun (name, convert) -> convert (part name)) fields
            unbox (ctor values)

        static member ResourceToString (resource : 'Resource) : string =
            let values = reader resource
            let template = UriTemplate(urls.[0])
            
            for name in template.GetParameterNames() do
                let value =
                    match Array.tryFindIndex (fst >> (=) name) fields with
                    | Some n -> values.[n]
                    | None -> failwithf "No field called '%s' in %O" name typeof<'Resource>

                template.SetParameter(name, value)

            template.Resolve()

    let private addRoute<'Resource, 'Response when 'Resource :> IRestResource> (t : NancyModule) (builder : NancyModule.RouteBuilder) (fn : 'Resource -> 'Response) (url : string) =
        let path, queryParams =
            let template = UriTemplate(url)
            for name in template.GetParameterNames() do
                template.SetParameter(name, "{" + name + "}")
                
            match template.Resolve().Replace("%7B", "{").Replace("%7D", "}").Split([| '?' |], 2) with
            | [| |] -> "", Set.empty
            | [| path |] -> path, Set.empty
            | a -> a.[0], Set.ofSeq (HttpUtility.ParseQueryString(a.[1])).AllKeys

        builder.[path] <- fun dict ->
            let dict : DynamicDictionary = unbox dict
            let query : DynamicDictionary = unbox t.Request.Query

            let resource =
                RestResourceImpl<_>.PartsToResource <| fun name ->
                    let dict =
                        if Set.contains name queryParams then
                            query
                        else
                            dict

                    unbox (dict.[name] :?> DynamicDictionaryValue).Value

            box (fn resource)

    [<NoDynamicInvocation>]
    let inline private retype (x : 'a) : 'b = (# "" x : 'b #)

    type NancyModule with
        member t.UrlFor (resource : 'Resource) : string<'Resource> =
            retype (t.ModulePath + RestResourceImpl<_>.ResourceToString(resource))

        member t.GetT<'Resource, 'Response when 'Resource :> IRestGet<'Response>> (fn : 'Resource -> 'Response) =
            Array.iter (addRoute t t.Get fn) RestResourceImpl<'Resource>.Urls

        member t.PutT<'Resource, 'Request, 'Response when 'Resource :> IRestPut<'Request, 'Response>> (fn : 'Resource -> 'Request -> 'Response) =
            Array.iter (addRoute t t.Put (fun resource -> fn resource (t.Bind<_>()))) RestResourceImpl<'Resource>.Urls

        member t.PostT<'Resource, 'Request, 'Response when 'Resource :> IRestPost<'Request, 'Response>> (fn : 'Resource -> 'Request -> 'Response) =
            Array.iter (addRoute t t.Post (fun resource -> fn resource (t.Bind<_>()))) RestResourceImpl<'Resource>.Urls

        member t.DeleteT<'Resource when 'Resource :> IRestDelete> (fn : 'Resource -> unit) =
            Array.iter (addRoute t t.Delete fn) RestResourceImpl<'Resource>.Urls

type StringModelBinder() =
    interface IModelBinder with
        member t.CanBind typ =
            typ = typeof<string>
            
        member t.Bind(context, modelType, instance, config, blacklist) =
            let bytes = Array.zeroCreate (int context.Request.Headers.ContentLength)
            let length = context.Request.Body.Read(bytes, 0, bytes.Length)
            box (Encoding.UTF8.GetString(bytes, 0, length))
