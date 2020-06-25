module Model

[<Measure>] type rel
[<Measure>] type pixel
[<Measure>] type second

type Vector<[<Measure>] 'T> = {
    X: float<'T>
    Y: float<'T>
} with
    static member Vec(x: float<'T>, y: float<'T>) = { X = x; Y = y }

    static member (+) (a: Vector<'T>, b: Vector<'T>) = Vector.Vec(a.X + b.X, a.Y + b.Y)
    static member (~-) (a: Vector<'T>) = Vector.Vec(-a.X, -a.Y)
    static member (-) (a: Vector<'T>, b: Vector<'T>) = Vector.Vec(a.X - b.X, a.Y - b.Y)

    static member (*) (a: Vector<'T>, b: float<'U>) = {X = a.X * b; Y = a.Y * b}
    static member (*) (a: float<'U>, b: Vector<'T>) = {X = a * b.X; Y = a * b.Y}

    static member (*) (a: Vector<'T>, b: Vector<'U>) = a.X * b.X + a.Y * b.Y
    
    member this.Magnitude = sqrt (this * this)
    member this.Norm = (1. / this.Magnitude) * this

    static member Zero : Vector<'T> = { X = 0.0<_>; Y = 0.0<_> }
    member this.IsZero = (this = Vector.Zero)

let vec (x: float<'T>, y: float<'T>) = { X = x; Y = y }
let proj (l: Vector<'U>) (a: Vector<'T>) = (a * l) / (l * l) * l

type PhysicsBall = {
    Center: Vector<rel>
    Velocity: Vector<rel/second>
    Radius: float<rel>
} with
    member this.Tick (s: float<second>) = { this with Center = this.Center + this.Velocity * s }
    member this.Collide (segment: Vector<rel>) = 
        let v = this.Velocity
        let pr = proj segment v
        let ort = v - pr
        { this with Velocity = pr - ort }
    member this.MaybeCollide (segment: Vector<rel> option) =
        match segment with
        | None -> this
        | Some v -> this.Collide v

type ColliderSegment = {
    A: Vector<rel>
    B: Vector<rel>
} with
    member this.Intersects (ball: PhysicsBall) =
        let ab = this.B - this.A
        let ac = ball.Center - this.A
        let ad = proj ab ac
        0.<rel^2> <= ad * ab && ad * ab <= ab * ab && (ac - ad).Magnitude <= ball.Radius

type ColliderRectangle = {
    A: Vector<rel>
    C: Vector<rel>
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
                    (Vector<rel>.Zero) this.Segments
        if v.IsZero then None else Some v
    member this.Contains (p: Vector<rel>) =
        let minx = min this.A.X this.C.X
        let miny = min this.A.Y this.C.Y
        let maxx = max this.A.X this.C.X
        let maxy = max this.A.Y this.C.Y
        minx <= p.X && p.X <= maxx && miny <= p.Y && p.Y <= maxy

[<RequireQualifiedAccess>]
type Message = Tick | MouseMove of Vector<rel> | Click

[<RequireQualifiedAccess>]
type GameState = Halt | Running

type Model = {
    Ball: PhysicsBall
    Paddle: ColliderRectangle
    Border: ColliderRectangle
    
    State: GameState
    LastTick: System.DateTime
    
    Targets: (ColliderRectangle * int) list
}
