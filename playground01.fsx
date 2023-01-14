#load "Assert.fsx"

[<AbstractClass>]
type Expr(value: float) =
  // Local gradient of this node
  member val Grad = 0.0 with get, set
  // Numerical result of the computation represented by this expression
  member val Value = value
  // Mutates local gradients of expressions this expression depends on,
  // i.e. "how much should expression I depend upon change to make my value go up by 1 unit?"
  abstract member UpdateGradient: unit -> unit
  abstract member Children: Expr seq

  member inline this.AddGrad x =
    this.Grad <- this.Grad + x

  member this.Backward() =
    this.Grad <- 1.0
    let mutable res = []
    let visited = System.Collections.Generic.HashSet<_>()
    let rec loop (v: Expr) =
      if visited.Add v then
        for child in v.Children do
          loop child
        res <- v::res
    loop this

    for v in res do
      v.UpdateGradient()


type Value(value: float) =
  inherit Expr(value)
  override _.UpdateGradient() = ()
  override _.Children = Seq.empty

type Mult(left: Expr, right: Expr) =
  inherit Expr(left.Value * right.Value)
  override this.UpdateGradient() =
    // z = x * y; x = f(a)
    // dz/da = dz/dx * dx/da = y * dx/da
    left.AddGrad <| right.Value * this.Grad
    right.AddGrad <| left.Value * this.Grad
  override _.Children = seq {left; right}

type Add(left: Expr, right: Expr) =
  inherit Expr(left.Value + right.Value)
  override this.UpdateGradient() =
    // z = x + y; x = f(a)
    // dz/da = dz/dx * dx/da = 1 * dx/da = dx/da
    left.AddGrad this.Grad
    right.AddGrad this.Grad
  override _.Children = seq {left; right}

type Tanh(x: Expr) =
  inherit Expr(x.Value |> tanh)
  override this.UpdateGradient() =
    // y = tanh(x); x = f(a)
    // dy/da = dy/dx * dx/da = (1 - tanh(x)^2) * dx/da
    x.AddGrad <| (1.0 - this.Value ** 2) * this.Grad
  override _.Children = seq {x}

type Exp(x: Expr) =
  inherit Expr(x.Value |> exp)
  override this.UpdateGradient() =
    x.AddGrad <| this.Value * this.Grad
  override _.Children = seq {x}

type Pown(x: Expr, n: int) =
  inherit Expr(pown (x.Value) n)
  override this.UpdateGradient() =
    x.AddGrad <| float n * pown (x.Value) (n - 1) * this.Grad
  override _.Children = seq {x}

type Pow(x: Expr, n: float) =
  inherit Expr((x.Value) ** n)
  override this.UpdateGradient() =
    x.AddGrad <| n * (x.Value) ** (n - 1.0) * this.Grad
  override _.Children = seq {x}

type Expr
with
  static member inline (*) (left, right) = Mult(left, right)
  static member inline (*) (left: Expr, right: float) = Mult(left, Value(right))
  static member inline (*) (left: float, right: Expr) = Mult(Value(left), right)

  static member inline (+) (left: #Expr, right: #Expr) = Add(left, right)
  static member inline (+) (left: float, right: #Expr) = Add(Value(left), right)
  static member inline (+) (left: #Expr, right: float) = Add(left, Value(right))

  static member inline (/) (left: #Expr, right: #Expr) = Mult(left, Pown(right, -1))

  // Have to specify return types explicitly because F# compiler requires result type to match 1st param type
  static member inline Pow (left, right): Expr = Pown(left, right)
  // Have to specify return types explicitly because F# compiler requires result type to match 1st param type
  static member inline Pow (left, right): Expr = Pow(left, right)

  // Have to specify return types explicitly because F# compiler requires result type to match 1st param type
  static member inline Exp left: Expr = Exp(left)

let roundFloat x = System.Double.Round(x, 3)

let test1 =
  let a, b, c = Value(2), Value(-3), Value(10)
  let e = a * b
  let d = e + c
  let f = Value(-2)
  let L = d * f
  L.Backward()

  Assert.eq
    ([a :> Expr; b; c; e; d; f; L] |> List.map (fun x -> x.Grad))
    [6.0; -4.0; -2.0; -2.0; -2.0; 4.0; 1.0]

let test2 =
  let x1, x2 = Value(2), Value(0)
  let w1, w2 = Value(-3), Value(1)
  let b = Value(6.8813735870195432)
  let x1w1, x2w2 = x1 * w1, x2 * w2
  let x1w1x2w2 = x1w1 + x2w2
  let n = x1w1x2w2 + b
  let o = Tanh(n)
  o.Backward()

  Assert.eq
    ([x1 :> Expr; x2; w1; w2; b; x1w1; x2w2; x1w1x2w2; n; o] |> List.map (fun x -> System.Double.Round(x.Grad, 3)))
    [-1.5; 0.5; 1.0; 0.0; 0.5; 0.5; 0.5; 0.5; 0.5; 1.0]

let test3 =
  let a = Value(1)
  let b = a + a
  b.Backward()

  Assert.eq (a.Grad) 2.0

let test4 =
  let a = Value(1.0)
  Assert.eq ((a + 1.0).Value) 2.0
  Assert.eq ((2.0 + a).Value) 3.0

let test5 =
  let a = Value(2.0)
  let b = exp (a :> Expr)
  Assert.eq (roundFloat b.Value) (roundFloat 7.389056099)

let test6 =
  let a = Value(2.0)
  let b = (a :> Expr) ** 2
  let c = (a :> Expr) ** 3
  Assert.eq (b.Value) 4
  Assert.eq (c.Value) 8
