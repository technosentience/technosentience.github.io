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

    let ball =
        ball.ProcessCollider model.Border

    let ball =
        ball.ProcessCollider model.Paddle

    processTargets { model with Ball = ball }

let updateState (model: Model) =
    if model.DeadArea.Intersects model.Ball then { model with State = GameState.Lost }
    else if model.Targets.IsEmpty then { model with State = GameState.Won }
    else model

let gameUpdate (msg: Message) (model: Model) =
    match msg with
    | Message.MouseMove v ->
        let c = model.Paddle.Center

        let x =
            max v.X (model.Paddle.Width * 0.5)
            |> min (150. - model.Paddle.Width * 0.5)

        let paddle =
            { model.Paddle with
                  Center = (vec (x, c.Y)) }

        { model with Paddle = paddle }

    | Message.Tick ->
        model
        |> updateBall
        |> updateCollisions
        |> updateState
    | _ -> model

let update (msg: Message) (model: Model) =
    let m =
        (match msg with
         | Message.Click ->
             if model.State <> GameState.Running then
                 { init () with
                       State = GameState.Running }
             else
                 model
         | _ -> model)

    if m.State = GameState.Running then gameUpdate msg m else m
