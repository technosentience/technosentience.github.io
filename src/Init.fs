module Init
open Model
open View
open Update

let initBall : PhysicsBall = {
    Center = vec (75.<rel>, 50.<rel>)
    Velocity = 10. * vec (-1.<rel/second>, 1.<rel/second>)
    Radius = 1.<rel>
}

let initPaddle : ColliderRectangle = {
    A = vec (75.<rel> - pixelToRel(60.<pixel>), 80.<rel> - pixelToRel(10.<pixel>))
    C = vec (75.<rel> + pixelToRel(60.<pixel>), 80.<rel> + pixelToRel(10.<pixel>))
}

let targets = [
        for y = 1 to 7 do
            for x = 1 to 10 do
                yield { 
                    A = vec (float(x - 1) * 15.<rel> + pixelToRel(20.<pixel>), float(y - 1) * 7.<rel> + pixelToRel(20.<pixel>))
                    C = vec (float(x) * 15.<rel> - pixelToRel(20.<pixel>), float(y) * 7.<rel> - pixelToRel(20.<pixel>))
                }, y
    ]

let init () : Model = {
    Ball = initBall
    Paddle = initPaddle
    Border = { A = vec (1.<rel>, 1.<rel>); C = vec (149.<rel>, 99.<rel>)}

    State = GameState.Halt
    LastTick = System.DateTime.Now

    Targets = targets
}
