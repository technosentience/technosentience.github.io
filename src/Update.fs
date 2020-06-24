module Update
open Model

let updateBall (model: Model)  = 
    let delta = 60. / 1000.
    let ball = model.Ball.Tick delta
    { model with Ball = ball }

let processCollision (ball: PhysicsBall) (collider: ColliderSegment) =
    if collider.Intersects ball then 
        ball.Collide (collider.B - collider.A)
    else ball

let processTargets (model: Model) =
    let mutable ball = model.Ball   // TODO: rewrite
    let mutable c = model.TargetsLeft
    for y = 1 to 7 do
        for x = 1 to 10 do
            if model.TargetsActive.[(y - 1) * 10 + x - 1] then    
                let cv = model.Targets.[(y - 1) * 10 + x - 1].CollisionVector ball
                if cv <> vec (0., 0.) then
                    ball <- ball.Collide cv
                    model.TargetsActive.[(y - 1) * 10 + x - 1] <- false
                    c <- c - 1
    { model with Ball = ball; TargetsLeft = c }

let updateCollisions (model: Model) =
    let ball = model.Ball
    let ball = ball.Collide (model.Border.CollisionVector ball)
    let ball = ball.Collide (model.Paddle.CollisionVector ball)
    processTargets { model with Ball = ball }

let update (msg: Message) (model: Model) = 
    match msg with
    | Message.MouseMove v ->
        let a, c = model.Paddle.A, model.Paddle.C
        let mov = { (v - 0.5 * (a + c)) with Y = 0. }
        let paddle = { A = mov + a; C = mov + c }
        { model with Paddle = paddle }
    | Message.Tick ->
        model |> updateBall |> updateCollisions
    | _ -> model
