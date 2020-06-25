module View
open Model

open Fable.Core.JsInterop
open Browser
open Elmish

let canvWidth = window.innerWidth * 0.9<pixel>
let canvHeight = window.innerHeight * 0.9<pixel>

let gameAreaWidth = canvWidth * 0.8
let gameAreaHeight = canvWidth / 1.5

let relHeight = 100.<rel>
let relWidth = 150.<rel>

let relToPixel (r: float<rel>) = r * (gameAreaHeight / relHeight)
let relToPixelV (r: Vector<rel>) = { X = relToPixel r.X; Y = relToPixel r.Y}

let pixelToRel (p: float<pixel>) = p * (relHeight / gameAreaHeight)
let pixelToRelV (p: Vector<pixel>) = { X = pixelToRel p.X; Y = pixelToRel p.Y}

let canvas = document.getElementById("canvas") :?> Types.HTMLCanvasElement
let ctx = canvas.getContext_2d()

canvas.width <- canvWidth * 1.<1/pixel>
canvas.height <- canvHeight * 1.<1/pixel>

let colorRect (color : string) (x, y, w, h) =
    System.Console.WriteLine(color)
    printfn "%A %A %A %A" x y w h
    ctx.fillStyle <- !^ color
    ctx.fillRect(x * 1.<1/pixel>, y * 1.<1/pixel>, w * 1.<1/pixel>, h * 1.<1/pixel>)

let strokeRect (color : string) (x, y, w, h) =
    ctx.strokeStyle <- !^ color
    ctx.strokeRect(x * 1.<1/pixel>, y * 1.<1/pixel>, w * 1.<1/pixel>, h * 1.<1/pixel>)

let colorCircle (color: string) (x, y, r) =
    ctx.beginPath()
    ctx.arc(x * 1.<1/pixel>, y * 1.<1/pixel>, r * 1.<1/pixel>, 0., 2. * System.Math.PI)
    ctx.fillStyle <- !^ color
    ctx.fill()

let bgColor = "white"
let drawBg () =
    colorRect bgColor (0.<_>, 0.<_>, canvWidth, canvHeight)

let borderColor = "gray"
let drawBorder (border: ColliderRectangle) =
    let a, ac = relToPixelV border.A, relToPixelV (border.C - border.A)
    strokeRect borderColor (a.X, a.Y, ac.X, ac.Y)

let ballColor = "red"
let drawBall (ball: PhysicsBall) = 
    let c, r = relToPixelV ball.Center, relToPixel ball.Radius
    colorCircle ballColor (c.X, c.Y, r)

let paddleColor = "black"
let drawPaddle (paddle: ColliderRectangle) =
    let a, ac = relToPixelV paddle.A, relToPixelV (paddle.C - paddle.A)
    colorRect paddleColor (a.X, a.Y, ac.X, ac.Y)

let rainbow = [|"red"; "orange"; "yellow"; "green"; "blue"; "indigo"; "violet"|]
let drawTargets (targets: (ColliderRectangle * int) list)  =
    for (t, y) in targets do
        let a, ac = relToPixelV t.A, relToPixelV (t.C - t.A)
        colorRect (rainbow.[y - 1]) (a.X, a.Y, ac.X, ac.Y)
        

let view (model: Model) (dispatch: Dispatch<Message>) = 
//    drawBg()
//    drawBorder(model.Border)
//    drawBall(model.Ball)
    drawPaddle(model.Paddle)
//    drawTargets(model.Targets)
