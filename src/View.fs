module View
open Model

open Fable.Core.JsInterop
open Browser
open Elmish

let width = window.innerWidth
let height = window.innerHeight

let canvas = document.getElementById("canvas") :?> Types.HTMLCanvasElement
let ctx = canvas.getContext_2d()

canvas.width <- width
canvas.height <- height

let bgColor = "white"
let drawBg () =
    ctx.fillStyle <- !^ bgColor
    ctx.fillRect(0., 0., width, height)

let ballColor = "red"
let drawBall (ball: PhysicsBall) = 
    let c, r = ball.Center, ball.Radius
    ctx.beginPath()
    ctx.arc(c.X, c.Y, r, 0., 2. * System.Math.PI)
    ctx.fillStyle <- !^ ballColor
    ctx.fill()

let paddleColor = "black"
let drawPaddle (paddle: ColliderRectangle) =
    let a, ac = paddle.A, paddle.C - paddle.A
    ctx.fillStyle <- !^ paddleColor
    ctx.fillRect(a.X, a.Y, ac.X, ac.Y)

let view (model: Model) (dispatch: Dispatch<Message>) = 
    drawBg()
    drawBall(model.Ball)
    drawPaddle(model.Paddle)
