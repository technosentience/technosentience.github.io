module Update

open Model
open Init

let updateBall (model: Model) =
    let delta = (60. / 1000.) * 1.
    let ball = model.Ball.Tick delta
    { model with Ball = ball }


let processTargets (model: Model) =
    let rec f (acc: PhysicsBall * (Rectangle * int) list) (li: (Rectangle * int) list) =
        match li with
        | [] -> acc
        | head :: tail ->
            let (b, pr) = acc
            let i = (fst head :> ICollider).CollisionInfo b
            match i with
            | Some i -> f (b.Collide i, pr) tail
            | None -> f (b, pr @ [ head ]) tail

    let ball, targets = f (model.Ball, []) model.Targets
    { model with
          Ball = ball
          Targets = targets }

let updateCollisions (model: Model) =
    let ball = model.Ball

    let ball = ball.ProcessCollider model.Border

    let ball = ball.ProcessCollider model.Paddle

    processTargets { model with Ball = ball }

let updateState (model: Model) =
    if model.DeadArea.Intersects model.Ball then { model with State = GameState.Lost }
    else if model.Targets.IsEmpty then { model with State = GameState.Won }
    else model

let updatePaddle (cursor: Vector) (model: Model) =
    let c = model.Paddle.Center

    let x =
        clamp (model.Paddle.Width * 0.5) (150. - model.Paddle.Width * 0.5) cursor.X

    let paddle =
        { model.Paddle with
              Center = (vec (x, c.Y)) }

    { model with Paddle = paddle }

let onClick (model: Model) =
    if model.State <> GameState.Running then
        { init () with
              State = GameState.Running }
    else
        model

let gameUpdate (msg: Message) (model: Model) =
    match msg with
    | Message.MouseMove v -> updatePaddle v model
    | Message.Tick ->
        model
        |> updateBall
        |> updateCollisions
        |> updateState
    | _ -> model

let update (msg: Message) (model: Model) =
    if model.State = GameState.Running then gameUpdate msg model
    else if msg = Message.Click then onClick model
    else model
