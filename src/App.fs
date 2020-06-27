module App
open Model
open View
open Update
open Init
open Input

open Elmish

printfn "%A" (pixelToRel 20.)

Program.mkSimple init update view
|> Program.withSubscription mouse
|> Program.withSubscription timer
|> Program.run
