module View
open Model

open Fable.Core.JsInterop
open Browser
open Elmish

let canvWidth = window.innerWidth * 0.9
let canvHeight = window.innerHeight * 0.9

let gameAreaWidth = canvWidth * 0.8
let gameAreaHeight = canvWidth / 1.5

let relHeight = 100.
let relWidth = 150.

let relToPixel (r: float) = r * (gameAreaHeight / relHeight)
let relToPixelV (r: Vector) = { X = relToPixel r.X; Y = relToPixel r.Y}

let pixelToRel (p: float) = p * (relHeight / gameAreaHeight)
let pixelToRelV (p: Vector) = { X = pixelToRel p.X; Y = pixelToRel p.Y}

let canvas = document.getElementById("canvas") :?> Types.HTMLCanvasElement
let ctx = canvas.getContext_2d()

canvas.width <- canvWidth * 1.
canvas.height <- canvHeight * 1.

let colorRect (color : string) (x, y, w, h) =
    System.Console.WriteLine(color)
    printfn "%A %A %A %A" x y w h
    ctx.fillStyle <- !^ color
    ctx.fillRect(x * 1., y * 1., w * 1., h * 1.)

let strokeRect (color : string) (x, y, w, h) =
    ctx.strokeStyle <- !^ color
    ctx.strokeRect(x * 1., y * 1., w * 1., h * 1.)

let colorCircle (color: string) (x, y, r) =
    ctx.beginPath()
    ctx.arc(x * 1., y * 1., r * 1., 0., 2. * System.Math.PI)
    ctx.fillStyle <- !^ color
    ctx.fill()

let bgColor = "white"
let drawBg () =
    colorRect bgColor (0., 0., canvWidth, canvHeight)

let borderColor = "gray"
let drawBorder (border: ColliderRectangle) =
    let a, ac = relToPixelV border.A, relToPixelV (border.C - border.A)
    strokeRect borderColor (a.X, a.Y, ac.X, ac.Y)

let ballColor = "red"
let drawBall (ball: PhysicsBall) = 
    let c, r = relToPixelV ball.Center, relToPixel ball.Radius
    colorCircle ballColor (c.X, c.Y, r)

let paddleColor = "black"
let drawPaddle (paddle: ColliderPaddle) =
    let c, w, h = relToPixelV paddle.Center, relToPixel paddle.Width, relToPixel paddle.Height
    let rw, rh = w - h, h
    let rx, ry = c.X - 0.5 * rw, c.Y - 0.5 * rh
    colorRect paddleColor (rx, ry, rw, rh)
    colorCircle paddleColor (rx, c.Y, 0.5 * rh)
    colorCircle paddleColor (rx + rw, c.Y, 0.5 * rh)

let rainbow = [|"red"; "orange"; "yellow"; "green"; "blue"; "indigo"; "violet"|]
let drawTargets (targets: (ColliderRectangle * int) list)  =
    for (t, y) in targets do
        let a, ac = relToPixelV t.A, relToPixelV (t.C - t.A)
        colorRect (rainbow.[y - 1]) (a.X, a.Y, ac.X, ac.Y)
        

let view (model: Model) (dispatch: Dispatch<Message>) = 
    drawBg()
    drawBorder(model.Border)
    drawBall(model.Ball)
    drawPaddle(model.Paddle)
    drawTargets(model.Targets)
