module App
open Model
open View
open Update

open Fable.Core
open Elmish

let initBall : PhysicsBall = {
    Center = vec (0.5 * width, 0.5 * height)
    Velocity = vec (0.0, 0.0)
    Radius = 10.
}

let initPaddle : ColliderRectangle = {
    A = vec (0.5 * width - 60., 0.8 * height - 10.)
    C = vec (0.5 * width + 60., 0.8 * height + 10.)
}

let init () : Model = {
    Ball = initBall
    Paddle = initPaddle
}

Program.mkSimple init update view |> Program.run
