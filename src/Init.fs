module Init
open Model
open View
open Update

let initBall : PhysicsBall = {
    Center = vec (75., 50.)
    Velocity = 10. * vec (-1., 1.)
    Radius = 1.
}

let initPaddle : ColliderRectangle = {
    A = vec (75. - pixelToRel(60.), 80. - pixelToRel(10.))
    C = vec (75. + pixelToRel(60.), 80. + pixelToRel(10.))
}

let targets = [
        for y = 1 to 7 do
            for x = 1 to 10 do
                yield { 
                    A = vec (float(x - 1) * 15. + pixelToRel(20.), float(y - 1) * 7. + pixelToRel(20.))
                    C = vec (float(x) * 15. - pixelToRel(20.), float(y) * 7. - pixelToRel(20.))
                }, y
    ]

let init () : Model = {
    Ball = initBall
    Paddle = initPaddle
    Border = { A = vec (1., 1.); C = vec (149., 99.)}

    State = GameState.Halt
    LastTick = System.DateTime.Now

    Targets = targets
}
