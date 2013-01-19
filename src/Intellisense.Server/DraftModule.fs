namespace Tim.TryFSharp.Intellisense.Server

open Nancy

[<Url("/hello/{who}")>]
type Hello = { Who : string } with interface IRestGet<string>

type HelloModule() as t =
   inherit NancyModule()

   do t.GetT <| fun (hello : Hello) ->
       sprintf "Hello, %s!" hello.Who
