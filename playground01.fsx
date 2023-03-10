#load "Assert.fsx"

[<AbstractClass>]
type Expr() as this =
  let getChildren () =
    let mutable res = []
    let visited = System.Collections.Generic.HashSet<_>()
    let rec loop (v: Expr) =
      if visited.Add v then
        for child in v.Children do
          loop child
        res <- v::res
    loop this
    res

  // Local gradient of this node
  member val Grad = 0.0 with get, set
  // Mutates local gradients of expressions this expression depends on,
  // i.e. "how much should expression I depend upon change to make my value go up by 1 unit?"
  abstract member UpdateGradient: unit -> unit
  abstract member Children: Expr seq
  // Numerical result of the computation represented by this expression
  abstract member Value: float

  override this.ToString() = $"{this.GetType()}: {this.Value}"

  member this.AddGrad x =
    this.Grad <- this.Grad + x

  member this.Backward() =
    this.Grad <- 1.0
    for v in getChildren() do
      v.UpdateGradient()

  member this.ZeroGrad() =
    for v in getChildren() do
      v.Grad <- 0.0

// Just a dumb Expr wrapper around value, most likely to be used as parameter or model input
type Value(initialValue: float) =
  inherit Expr()
  let mutable value = initialValue
  override _.UpdateGradient() = ()
  override _.Children = Seq.empty
  override _.Value with get() = value
  member _.SetValue(v) = value <- v

type Mult(left: Expr, right: Expr) =
  inherit Expr()
  override this.UpdateGradient() =
    // z = x * y; x = f(a)
    // dz/da = dz/dx * dx/da = y * dx/da
    left.AddGrad <| right.Value * this.Grad
    right.AddGrad <| left.Value * this.Grad
  override _.Children = seq {left; right}
  override _.Value = left.Value * right.Value

type Add(left: Expr, right: Expr) =
  inherit Expr()
  override this.UpdateGradient() =
    // z = x + y; x = f(a)
    // dz/da = dz/dx * dx/da = 1 * dx/da = dx/da
    left.AddGrad this.Grad
    right.AddGrad this.Grad
  override _.Children = seq {left; right}
  override _.Value = left.Value + right.Value

type Tanh(x: Expr) =
  inherit Expr()
  override this.UpdateGradient() =
    // y = tanh(x); x = f(a)
    // dy/da = dy/dx * dx/da = (1 - tanh(x)^2) * dx/da
    x.AddGrad <| (1.0 - this.Value ** 2) * this.Grad
  override _.Children = seq {x}
  override _.Value = x.Value |> tanh

type Exp(x: Expr) =
  inherit Expr()
  override this.UpdateGradient() =
    x.AddGrad <| this.Value * this.Grad
  override _.Children = seq {x}
  override _.Value = x.Value |> exp

type Pown(x: Expr, n: int) =
  inherit Expr()
  override this.UpdateGradient() =
    x.AddGrad <| float n * pown (x.Value) (n - 1) * this.Grad
  override _.Children = seq {x}
  override _.Value = pown (x.Value) n

type Pow(x: Expr, n: float) =
  inherit Expr()
  override this.UpdateGradient() =
    x.AddGrad <| n * (x.Value) ** (n - 1.0) * this.Grad
  override _.Children = seq {x}
  override _.Value = (x.Value) ** n

type Expr
with
  static member inline Wrap (value: float): Expr = Value(value)
  static member inline Tanh (value: Expr): Expr = Tanh(value)

  static member inline (*) (left, right): Expr = Mult(left, right)
  static member inline (*) (left: Expr, right: float): Expr = Mult(left, Value(right))
  static member inline (*) (left: float, right: Expr): Expr = Mult(Value(left), right)

  static member inline (+) (left: Expr, right: Expr): Expr = Add(left, right)
  static member inline (+) (left: float, right: Expr): Expr = Add(Value(left), right)
  static member inline (+) (left: Expr, right: float): Expr = Add(left, Value(right))

  static member inline (-) (left: Expr, right: Expr): Expr = Add(left, Mult(right, Value(-1)))

  static member inline (/) (left: Expr, right: Expr): Expr = Mult(left, Pown(right, -1))

  // Have to specify return types explicitly because F# compiler requires result type to match 1st param type
  static member inline Pow (left, right): Expr = Pown(left, right)
  // Have to specify return types explicitly because F# compiler requires result type to match 1st param type
  static member inline Pow (left, right): Expr = Pow(left, right)

  static member inline DivideByInt (left: Expr, right: int): Expr = Mult(left, Value(right ** -1))

  static member inline get_Zero (): Expr = Value(0.0)

  // Have to specify return types explicitly because F# compiler requires result type to match 1st param type
  static member inline Exp left: Expr = Exp(left)

let roundFloat x = System.Double.Round(x, 3)

// ----
// Expression tests
// ----

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

// ---
// Model
// ---

let random = new System.Random()
let inline randomFloat minValue maxValue = random.NextDouble() * (maxValue - minValue) - maxValue

// Just a single neuron cell: weight per input + own bias + non-linear activation
type Neuron(inputSize: int) =
  let weights = [| for _ in 1..inputSize -> randomFloat -1 1 |> Value |]
  let bias = Value(randomFloat -1 1)

  member _.Process (inputs: Expr seq): Expr =
    Seq.zip weights inputs
    |> Seq.fold (fun acc (w, x) -> acc + w * x) (bias :> Expr)
    |> Expr.Tanh

  member _.Parameters = seq {yield! weights; yield bias}

// Group of neurons, all reacting to the same input vector
type Layer(inputSize, neuronCount) =
  let neurons = [| for _ in 1..neuronCount -> Neuron(inputSize) |]

  member _.Process (inputs: Expr seq) =
    [| for n in neurons -> n.Process inputs |]

  member _.Parameters = seq { for n in neurons do yield! n.Parameters }

// Multi-layer perceptron, N inputs, M layers, with K[M] size of each layer
// Number of layer inputs:
//   - 1st layer: inputSize
//   - Nth layer: number of neurons at layer N-1
type MultiLayer(inputSize: int, layerSizes: int list) =
  let layers = [| for prev, next in Seq.pairwise(inputSize::layerSizes) -> Layer(prev, next) |]
  do Assert.eq (layers.Length) (layerSizes.Length)

  member _.Process (inputs: Expr seq) =
    let mutable current = inputs
    for layer in layers do
      current <- layer.Process current
    current

  member _.Parameters = seq { for l in layers do yield! l.Parameters }

// Mean squared error of ground truth vs predictions
let inline MSE (expected: Expr seq) (actual: Expr seq) =
  Seq.zip expected actual
  |> Seq.map (fun (e, a) -> (e - a) ** 2)
  |> Seq.average


// ---
// Model tests
// ---

let test7 =
  for _ in 1..1000 do
    let size = random.Next(1, 10)
    let out = [for _ in 1..size -> Value(randomFloat -100 100) :> Expr] |> Neuron(size).Process
    Assert.ok (out.Value >= -1 && out.Value <= 1)

let test8 =
  for _ in 1..1000 do
    let neuronCount = random.Next(2, 20)
    let neuronInputSize = random.Next(2, 20)
    let inputs = [for _ in 1..neuronInputSize -> Value(randomFloat -100 100) :> Expr]
    let outputs = Layer(neuronInputSize, neuronCount).Process(inputs)
    Assert.eq (outputs.Length) neuronCount
    for x in outputs do
      Assert.ok (x.Value >= -1 && x.Value <= 1)

let test9 =
  for _ in 1..1000 do
    let layerCount = random.Next(1, 10)
    let layerSizes = [for _ in 1..layerCount -> random.Next (2, 20)]
    let inputSize = random.Next(2, 20)
    let mlp = MultiLayer(inputSize, layerSizes)
    let inputs = [|for _ in 1..inputSize -> Value(randomFloat -100 100) :> Expr|]
    let outputs = mlp.Process inputs
    Assert.eq (Seq.length outputs) (Seq.last layerSizes)

let test10 =
  let mlp = MultiLayer(3, [4; 4; 1])
  let xs = [
      [2.0; 3.0; -1.0]
      [3.0; -1.0; 0.5]
      [0.5; 1.0; 1.0]
      [1.0; 1.0; -1.0]
    ]
  let ys = [1.0; -1.0; -1.0; 1.0] |> List.map Expr.Wrap
  for i in 1..300 do
    let predictions = [for x in xs -> x |> List.map Expr.Wrap |> mlp.Process |> Seq.head]
    let loss = MSE predictions ys
    printfn "Iteration %d, loss %f" i loss.Value
    loss.ZeroGrad()
    loss.Backward()
    for p in mlp.Parameters do
      p.SetValue(p.Value - p.Grad * 0.1)

  for i, x in Seq.indexed xs do
    let actual = mlp.Process (x |> List.map (Expr.Wrap)) |> Seq.head
    let expected = ys[i]
    let diff = abs(actual.Value - expected.Value)
    Assert.ok (diff <= 0.05)
