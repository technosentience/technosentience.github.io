module App
open Model
open View
open Update

open Browser
open Elmish

let initBall : PhysicsBall = {
    Center = vec (0.5 * width, 0.5 * height)
    Velocity = 150. * vec (-1., 1.)
    Radius = 10.
}

let initPaddle : ColliderRectangle = {
    A = vec (0.5 * width - 60., 0.8 * height - 10.)
    C = vec (0.5 * width + 60., 0.8 * height + 10.)
}

let init () : Model = {
    Ball = initBall
    Paddle = initPaddle
    Border = { A = vec (width * 0.05, height * 0.05); C = vec (width * 0.95, height * 0.95)}
    State = GameState.Halt
    LastTick = System.DateTime.Now
}

let timer (_: Model) = 
    let sub dispatch =
        let f _ = dispatch (Message.Tick(System.DateTime.Now))
        window.setInterval(f, 1000 / 60, []) |> ignore
    Cmd.ofSub sub

let mouse (_: Model) =
    let sub dispatch =
        let f (e: Types.MouseEvent) = dispatch (Message.MouseMove(vec (e.x, e.y)))
        let g (_: Types.MouseEvent) = dispatch Message.Click
        window.onmousemove <- f
        window.onclick <- g
    Cmd.ofSub sub

Program.mkSimple init update view
|> Program.withSubscription mouse
|> Program.withSubscription timer
|> Program.withConsoleTrace
|> Program.run
