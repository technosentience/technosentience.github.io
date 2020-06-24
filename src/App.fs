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

let targets : ColliderRectangle[] = [|
        for y = 1 to 7 do
            for x = 1 to 10 do
                yield { 
                    A = vec (float(x - 1) * width / 10. + 20., float(y - 1) * height / 15. + 20.)
                    C = vec (float(x) * width / 10. - 20., float(y) * height / 15.)
                }
    |]

let targetsActive = Array.create 70 true

let init () : Model = {
    Ball = initBall
    Paddle = initPaddle
    Border = { A = vec (width * 0.01, height * 0.01); C = vec (width * 0.99, height * 0.99)}

    State = GameState.Halt
    LastTick = System.DateTime.Now

    Targets = targets
    TargetsActive = targetsActive
    TargetsLeft = 70
}

let timer (_: Model) = 
    let sub dispatch =
        let f _ = dispatch Message.Tick
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
//|> Program.withConsoleTrace
|> Program.run
