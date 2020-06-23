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

let updateCollisions (model: Model) =
    let ball = model.Ball
    let ball = List.fold processCollision ball model.Border.Segments
    let ball = List.fold processCollision ball model.Paddle.Segments
    { model with Ball = ball }

let update (msg: Message) (model: Model) = 
    match msg with
    | Message.MouseMove v ->
        let a, c = model.Paddle.A, model.Paddle.C
        let mov = { (v - 0.5 * (a + c)) with Y = 0. }
        let paddle = { A = mov + a; C = mov + c }
        { model with Paddle = paddle }
    | Message.Tick _ ->
        model |> updateBall |> updateCollisions
    | _ -> model
