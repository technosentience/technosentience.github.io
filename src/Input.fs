module Input
open Model
open View
open Update

open Browser
open Elmish

let timer (_: Model) = 
    let sub dispatch =
        let f _ = dispatch Message.Tick
        window.setInterval(f, 1000 / 60, []) |> ignore
    Cmd.ofSub sub

let mouse (_: Model) =
    let sub dispatch =
        let f (e: Types.MouseEvent) = 
            dispatch (Message.MouseMove(pixelToRelV(1.<pixel> * vec (e.x, e.y))))
        let g (_: Types.MouseEvent) = dispatch Message.Click
        window.onmousemove <- f
        window.onclick <- g
    Cmd.ofSub sub
