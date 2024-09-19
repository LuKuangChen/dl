open Utilities

type input = (bool, bool)
type output = bool
type dist_output = float // that is in [0, 1]
type datum = {input: input, output: output}
type dataset = array<datum>
type loss = float // that is in [0, +inf)

// the learning rate
let alpha = 10.0

let dataset = [
  {input: (true, true), output: true},
  {input: (true, false), output: false},
  {input: (false, true), output: false},
  {input: (false, false), output: false},
]

let inject: bool => float = b => {
  if b {
    1.0
  } else {
    0.0
  }
}

let project = i => {
  if i == 0.0 {
    false
  } else if i == 1.0 {
    true
  } else {
    failwith("bad input")
  }
}

type parameters1 = {
  w1: float,
  w2: float,
  b: float,
}
let makeInitParameters = (): parameters1 => {
  w1: Math.random() -. 0.5,
  w2: Math.random() -. 0.5,
  b: Math.random() -. 0.5,
}
let displayParameter = ({b, w1, w2}) => {
  `y = ${b->Float.toString} + ${w1->Float.toString}*x1 + ${w2->Float.toString}*x2`
}

type hidden1 = (float, float)

type hidden2 = float

let layer1 = (x: input): hidden1 => {
  let (x1, x2) = x
  (inject(x1), inject(x2))
}

let layer2 = ((x1, x2): hidden1, phi: parameters1): hidden2 => {
  phi.b +. phi.w1 *. x1 +. phi.w2 *. x2
}

let d_layer2_b = (_x: hidden1, _phi: parameters1) => 1.0
let d_layer2_w1 = ((x1, _x2): hidden1, _phi: parameters1) => {
  x1->jitter
}
let d_layer2_w2 = ((_x1, x2): hidden1, _phi: parameters1) => {
  x2->jitter
}

let layer3 = (x: hidden2): dist_output => {
  sigmoid(x)
}

let d_layer3 = x => d_sigmoid(x)->jitter

// Use the cross-entropy as the loss function

let point_loss = (fact: output, pred: dist_output): float => {
  -.Math.log(
    if fact {
      pred
    } else {
      1.0 -. pred
    },
  )
}
assert(point_loss(true, 1.0) == 0.0)
assert(point_loss(false, 0.0) == 0.0)
assert(point_loss(true, 0.0) == Float.Constants.positiveInfinity)
assert(point_loss(false, 1.0) == Float.Constants.positiveInfinity)

let d_point_loss_pred = (fact: output, pred: dist_output): float => {
  bound(
    -.(
      if fact {
        1.0 /. pred
      } else {
        1.0 /. (pred -. 1.0)
      }
    ),
  )
}

let loss = (parameters, dataset): loss => {
  sum(
    dataset->Array.map(({input, output}) => {
      let h1 = input->layer1
      let h2 = h1->layer2(parameters)
      let pred = h2->layer3
      point_loss(output, pred)
    }),
  )
}

let d_loss_parameters = (parameters, dataset) => {
  let ds = dataset->Array.map(({input, output}) => {
    // Console.log2("Input:", input)
    let h1 = input->layer1
    // Console.log2("Hidden1:", h1)
    let h2 = h1->layer2(parameters)
    // Console.log2("Hidden2:", h2)
    let pred = h2->layer3
    // Console.log2("Prediction:", pred)
    // Console.log2("Actual:", output)
    let _point_loss = point_loss(output, pred)
    // Console.log2("Loss:", point_loss)
    let d_loss_pred = d_point_loss_pred(output, pred)
    // Console.log2("d Loss / d pred:", d_loss_pred)
    let d_pred_h2 = d_layer3(h2)
    // Console.log2("d Pred / d h2:", d_pred_h2)
    {
      b: bound(d_layer2_b(h1, parameters) *. d_pred_h2 *. d_loss_pred),
      w1: bound(d_layer2_w1(h1, parameters) *. d_pred_h2 *. d_loss_pred),
      w2: bound(d_layer2_w2(h1, parameters) *. d_pred_h2 *. d_loss_pred),
    }
  })
  {
    b: ds->Array.map(({b}) => b)->sum,
    w1: ds->Array.map(({w1}) => w1)->sum,
    w2: ds->Array.map(({w2}) => w2)->sum,
  }
}

let closeEnough = (p1, p2) => {
  sum([
    Math.pow(p1.b -. p2.b, ~exp=2.0),
    Math.pow(p1.w1 -. p2.w1, ~exp=2.0),
    Math.pow(p1.w2 -. p2.w2, ~exp=2.0),
  ]) <= Float.Constants.epsilon
}

let learn = (iteration: int, dataset) => {
  if !nonnegative(iteration) {
    failwith("bad iteration")
  }
  let updateParameter = parameters => {
    let d = d_loss_parameters(parameters, dataset)
    let {b, w1, w2} = parameters
    {
      b: b -. d.b *. alpha,
      w1: w1 -. d.w1 *. alpha,
      w2: w2 -. d.w2 *. alpha,
    }
  }
  let n = ref(iteration)
  let shouldBreak = ref(false)
  let currParameter = ref(makeInitParameters())
  while n.contents >= 0 && !shouldBreak.contents {
    Console.log(currParameter.contents)
    Console.log2("Loss =", loss(currParameter.contents, dataset))
    n := n.contents - 1
    let nextParameter = updateParameter(currParameter.contents)
    if closeEnough(currParameter.contents, nextParameter) {
      shouldBreak := true
    } else {
      currParameter := nextParameter
    }
  }
}

learn(1000, dataset)
