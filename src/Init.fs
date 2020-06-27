module Init
open Model

let rng = System.Random()

let initBall () : PhysicsBall = {
    Center = vec (75., 50.)
    Velocity = let a = (0.25 + 0.5 * rng.NextDouble()) * System.Math.PI in 20. * vec (cos a, -sin a)
    Radius = 1.
    CollisionTimeout = 0
}

let initPaddle : ColliderPaddle = {
    Center = vec (75., 80.)
    Width = 12.
    Height = 2.
}

let targets = [
        for y = 1 to 7 do
            for x = 1 to 10 do
                let cx = 1.8 + float(x - 1) * (150. - 2. * 1.8) / 10.
                let cy = 1.8 + float(y - 1) * 100. / 15.
                let cx2 = 1.8 + float(x) * (150. - 2. * 1.8) / 10.
                let cy2 = 1.8 + float(y) * 100. / 15.
                yield { 
                    A = vec (cx, cy)
                    C = vec (cx2, cy2)
                }, y
    ]

let init () : Model = {
    Ball = initBall ()
    Paddle = initPaddle
    Border = { A = vec (1., 1.); C = vec (149., 99.)}
    DeadArea = { A = vec (0., 95.); B = vec (150., 95.)}

    State = GameState.Halt
    LastTick = System.DateTime.Now

    Targets = targets
}
