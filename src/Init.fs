module Init
open Model
open View
open Update

let rng = System.Random()

let initBall : PhysicsBall = {
    Center = vec (75., 50.)
    Velocity = let a = rng.NextDouble() * System.Math.PI in 20. * vec (cos a, -sin a)
    Radius = 1.
}

let initPaddle : ColliderPaddle = {
    Center = vec (75., 80.)
    Width = pixelToRel(120.)
    Height = pixelToRel(20.)
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
