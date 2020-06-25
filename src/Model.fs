module Model

type Vector = {
    X: float
    Y: float
} with
    static member Vec(x: float, y: float) = { X = x; Y = y }

    static member (+) (a: Vector, b: Vector) = Vector.Vec(a.X + b.X, a.Y + b.Y)
    static member (~-) (a: Vector) = Vector.Vec(-a.X, -a.Y)
    static member (-) (a: Vector, b: Vector) = Vector.Vec(a.X - b.X, a.Y - b.Y)

    static member (*) (a: Vector, b: float) = {X = a.X * b; Y = a.Y * b}
    static member (*) (a: float, b: Vector) = {X = a * b.X; Y = a * b.Y}

    static member (*) (a: Vector, b: Vector) = a.X * b.X + a.Y * b.Y
    
    member this.Magnitude = sqrt (this * this)
    member this.Norm = (1. / this.Magnitude) * this

    static member Zero : Vector = { X = 0.0; Y = 0.0 }
    member this.IsZero = (this = Vector.Zero)

    member this.RotateLeft a = { X = this.X * cos a - this.Y * sin a; Y = this.X * sin a + this.Y * cos a }

let vec (x: float, y: float) = { X = x; Y = y }
let proj (l: Vector) (a: Vector) = (a * l) / (l * l) * l

type PhysicsBall = {
    Center: Vector
    Velocity: Vector
    Radius: float
} with
    member this.Tick (s: float) = { this with Center = this.Center + this.Velocity * s }
    member this.Collide (segment: Vector) = 
        let v = this.Velocity
        let pr = proj segment v
        let ort = v - pr
        { this with Velocity = pr - ort }
    member this.MaybeCollide (segment: Vector option) =
        match segment with
        | None -> this
        | Some v -> this.Collide v

type ColliderSegment = {
    A: Vector
    B: Vector
} with
    member this.Intersects (ball: PhysicsBall) =
        let ab = this.B - this.A
        let ac = ball.Center - this.A
        let ad = proj ab ac
        0. <= ad * ab && ad * ab <= ab * ab && (ac - ad).Magnitude <= ball.Radius

type ColliderRectangle = {
    A: Vector
    C: Vector
} with
    member this.B = { X = this.A.X; Y = this.C.Y }
    member this.D = { X = this.C.X; Y = this.A.Y }
    member this.Segments = [
        { A = this.A; B = this.B }
        { A = this.B; B = this.C }
        { A = this.C; B = this.D }
        { A = this.D; B = this.A }
    ]
    member this.CollisionVector (ball: PhysicsBall) =
        let v = List.fold 
                    (fun v (s: ColliderSegment) -> if s.Intersects ball then v + (s.B - s.A) else v)
                    (Vector.Zero) this.Segments
        if v.IsZero then None else Some v
    member this.Contains (p: Vector) =
        let minx = min this.A.X this.C.X
        let miny = min this.A.Y this.C.Y
        let maxx = max this.A.X this.C.X
        let maxy = max this.A.Y this.C.Y
        minx <= p.X && p.X <= maxx && miny <= p.Y && p.Y <= maxy

type ColliderPaddle = {
    Center: Vector
    Height: float
    Width: float
} with
    member this.MainSegment = {
        A = vec (this.Center.X - this.Width * 0.5, this.Center.Y - this.Height * 0.5)
        B = vec (this.Center.X + this.Width * 0.5, this.Center.Y - this.Height * 0.5)
    }
    member this.CollisionVector (ball: PhysicsBall) =
        if not (this.MainSegment.Intersects ball) then
            None
        else
            let v = this.MainSegment.A - this.MainSegment.B
            let ratio = abs (((ball.Center - this.MainSegment.A) * v) / (v * v))
            if ratio < 0.33 then
                Some (v.RotateLeft -(15. / 180. * System.Math.PI))
            else if ratio > 0.67 then
                Some(v.RotateLeft (15. / 180. * System.Math.PI))
            else Some v

[<RequireQualifiedAccess>]
type Message = Tick | MouseMove of Vector | Click

[<RequireQualifiedAccess>]
type GameState = Halt | Running

type Model = {
    Ball: PhysicsBall
    Paddle: ColliderPaddle
    Border: ColliderRectangle
    
    State: GameState
    LastTick: System.DateTime
    
    Targets: (ColliderRectangle * int) list
}
