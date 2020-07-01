module Model

let between l u x = l <= x && x <= u
let clamp l u x = max (min x u) l

type Vector =
    { X: float
      Y: float }
    static member (+)(a: Vector, b: Vector) = { X = a.X + b.X; Y = a.Y + b.Y }
    static member (~-)(a: Vector) = { X = -a.X; Y = -a.Y }
    static member (-)(a: Vector, b: Vector) = { X = a.X - b.X; Y = a.Y - b.Y }

    static member (*)(a: Vector, b: float) = { X = a.X * b; Y = a.Y * b }
    static member (*)(a: float, b: Vector) = { X = a * b.X; Y = a * b.Y }

    static member (*)(a: Vector, b: Vector) = a.X * b.X + a.Y * b.Y

    member this.Magnitude = sqrt (this * this)
    member this.Norm = (1. / this.Magnitude) * this

    static member Zero: Vector = { X = 0.0; Y = 0.0 }
    member this.IsZero = (this = Vector.Zero)

    member this.RotateLeft a =
        { X = this.X * cos a - this.Y * sin a
          Y = this.X * sin a + this.Y * cos a }

let vec (x: float, y: float) = { X = x; Y = y }

let proj (l: Vector) (v: Vector) = (v * l) / (l * l) * l
let ort (l: Vector) (v: Vector) = v - proj l v
let reflect (l: Vector) (v: Vector) = proj l v - ort l v

type CollisionInfo = { Line: Vector; Point: Vector }

type PhysicsBall =
    { Center: Vector
      Velocity: Vector
      Radius: float }
    member this.Tick(s: float) =
        { this with
              Center = this.Center + this.Velocity * s }

    member this.Collide(x: CollisionInfo) =
        { this with
              Center = x.Point // Retroactively move the ball to the point of collision
              Velocity = reflect x.Line this.Velocity }

type ICollider =
    abstract CollisionInfo: PhysicsBall -> CollisionInfo option

type PhysicsBall with
    member this.ProcessCollider(collider: ICollider) =
        match collider.CollisionInfo this with
        | None -> this
        | Some info -> this.Collide info

type Segment =
    { A: Vector
      B: Vector }
    member this.Intersects(ball: PhysicsBall) =
        let ab = this.B - this.A
        let ac = ball.Center - this.A
        let ad = proj ab ac // Point D is the projection of C on AB
        let cd = ad - ac
        (ad * ab |> between 0. (ab * ab)) // D is inside AB
        && (cd.Magnitude <= ball.Radius) // D is inside the circle

    interface ICollider with
        member this.CollisionInfo ball =
            let ab = this.B - this.A
            let ac = ball.Center - this.A
            let ad = proj ab ac
            let cd = ad - ac
            let c' = this.A + ad - cd.Norm * ball.Radius
            if this.Intersects ball then Some { Line = ab; Point = c' } else None


type Rectangle =
    { A: Vector
      C: Vector }
    member this.B = { X = this.A.X; Y = this.C.Y }
    member this.D = { X = this.C.X; Y = this.A.Y }

    member this.Segments =
        [ { A = this.A; B = this.B }
          { A = this.B; B = this.C }
          { A = this.C; B = this.D }
          { A = this.D; B = this.A } ]

    member this.Contains(p: Vector) =
        let minx = min this.A.X this.C.X
        let miny = min this.A.Y this.C.Y
        let maxx = max this.A.X this.C.X
        let maxy = max this.A.Y this.C.Y
        (between minx maxx p.X) && (between miny maxy p.Y)

    interface ICollider with
        member this.CollisionInfo ball =
            List.fold (fun s x -> if s = None then (x :> ICollider).CollisionInfo ball else s) None this.Segments

type Paddle =
    { Center: Vector
      Height: float
      Width: float }
    member this.MainSegment =
        { A = vec (this.Center.X - this.Width * 0.5, this.Center.Y - this.Height * 0.5)
          B = vec (this.Center.X + this.Width * 0.5, this.Center.Y - this.Height * 0.5) }

    interface ICollider with
        member this.CollisionInfo ball =
            let info =
                (this.MainSegment :> ICollider).CollisionInfo ball

            let v = this.MainSegment.A - this.MainSegment.B

            let ratio =
                abs (((ball.Center - this.MainSegment.A) * v) / (v * v))

            let angle =
                match ratio with
                | x when x < 0.33 -> -System.Math.PI * (15. / 180.)
                | x when x > 0.67 -> System.Math.PI * (15. / 180.)
                | _ -> 0.

            match info with
            | None -> None
            | Some info ->
                Some
                    { info with
                          Line = info.Line.RotateLeft angle }

[<RequireQualifiedAccess>]
type Message =
    | Tick
    | MouseMove of Vector
    | Click

[<RequireQualifiedAccess>]
type GameState =
    | Halt
    | Running
    | Lost
    | Won

type Model =
    { Ball: PhysicsBall
      Paddle: Paddle
      Border: Rectangle
      DeadArea: Segment

      State: GameState
      LastTick: System.DateTime

      Targets: (Rectangle * int) list }
