module Update
open Model

let updateBall (model: Model)  = 
    let delta = (60. / 1000.) * 1.<second>
    let ball = model.Ball.Tick delta
    { model with Ball = ball }

let processCollision (ball: PhysicsBall) (collider: ColliderSegment) =
    if collider.Intersects ball then 
        ball.Collide (collider.B - collider.A)
    else ball

let processTargets (model: Model) =
    let rec f (acc: PhysicsBall * (ColliderRectangle * int) list) (li: (ColliderRectangle * int) list) =
        match li with
        | [] -> acc
        | head :: tail ->
            let (b, pr) = acc
            let cv = (fst head).CollisionVector b
            match cv with
            | Some v -> f (b.Collide v, pr) tail
            | None -> f (b, pr @ [head]) tail

    let ball, targets = f (model.Ball, []) model.Targets
    { model with Ball = ball; Targets = targets }

let updateCollisions (model: Model) =
    let ball = model.Ball
    let ball = ball.MaybeCollide (model.Border.CollisionVector ball)
    let ball = ball.MaybeCollide (model.Paddle.CollisionVector ball)
    processTargets { model with Ball = ball }

let update (msg: Message) (model: Model) = 
    match msg with
    | Message.MouseMove v ->
        let a, c = model.Paddle.A, model.Paddle.C
        let mov = { (v - 0.5 * (a + c)) with Y = 0.<_> }
        let paddle = { A = mov + a; C = mov + c }
        if model.Border.Contains paddle.A && model.Border.Contains paddle.C then
            { model with Paddle = paddle }
        else model
    | Message.Tick ->
        model |> updateBall |> updateCollisions
    | _ -> model
