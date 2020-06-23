module Model

type Vector = {
    X: float
    Y: float
} with
    static member (+) (a: Vector, b: Vector) = {X = a.X + b.X; Y = a.Y + b.Y}
    static member (~-) (a: Vector) = { X = -a.X; Y = -a.Y}
    static member (-) (a: Vector, b: Vector) = {X = a.X - b.X; Y = a.Y - b.Y}

    static member (*) (a: Vector, b: float) = {X = a.X * b; Y = a.Y * b}
    static member (*) (a: float, b: Vector) = {X = a * b.X; Y = a * b.Y}

    static member (*) (a: Vector, b: Vector) = a.X * b.X + a.Y * b.Y
    
    member this.Magnitude = sqrt (this * this)
    member this.Norm = (1. / this.Magnitude) * this

let vec (x: float, y: float) = { X = x; Y = y }
let proj (l: Vector) (a: Vector) = (a * l) / (l * l) * l

type PhysicsBall = {
    Center: Vector
    Velocity: Vector
    Radius: float
} with
    member this.Tick s = { this with Center = this.Center + this.Velocity * s }
    member this.Collide (segment: Vector) = 
        let v = this.Velocity
        let pr = proj segment v
        let ort = v - pr
        { this with Velocity = pr - ort }

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

[<RequireQualifiedAccess>]
type Message = Tick of System.DateTime | MouseMove of Vector | Click

[<RequireQualifiedAccess>]
type GameState = Halt | Running

type Model = {
    Ball: PhysicsBall
    Paddle: ColliderRectangle
    Border: ColliderRectangle
    State: GameState
    LastTick: System.DateTime
}
