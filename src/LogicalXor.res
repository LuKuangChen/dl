open AutoDiff
open Utilities

type input = (bool, bool)
type output = bool
type dist_output = float // that is in [0, 1]
type datum = {input: input, output: output}
type dataset = array<datum>
type loss = float // that is in [0, +inf)

// the learning rate
let alpha = 1.0

let dataset = [
  {input: (true, true), output: false},
  {input: (true, false), output: true},
  {input: (false, true), output: true},
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

module MyTerm = MakeTerm()
module MyExtraOps = ExtraOperators(MyTerm)

let loss = dataset => {
  open! MyTerm
  open! MyExtraOps
  let w11 = claimMany(3)
  let w12 = claimMany(3)
  let w13 = claimMany(3)
  let w14 = claimMany(3)
  let w15 = claimMany(3)
  let w2 = claimMany(6)
  dataset
  ->Array.map(datum => {
    let {input, output} = datum
    let (x1, x2) = input
    let x = c(inject(x1))
    let y = c(inject(x2))
    let h11 = reELU(dotproduct([c(1.0), x, y], w11))
    let h12 = reELU(dotproduct([c(1.0), x, y], w12))
    let h13 = reELU(dotproduct([c(1.0), x, y], w13))
    let h14 = reELU(dotproduct([c(1.0), x, y], w14))
    let h15 = reELU(dotproduct([c(1.0), x, y], w15))
    let h2 = sigmoid(dotproduct([c(1.0), h11, h12, h13, h14, h15], w2))
    let pred = h2

    -log(
      if output {
        pred
      } else {
        c(1.0) - pred
      },
    )
  })
  ->Array.reduce(c(0.0), \"+")
}

let learn = (iteration: int, dataset) => {
  assert(iteration >= 0)
  let loss = loss(dataset)
  let n = ref(iteration)
  let shouldBreak = ref(false)
  let currParameter = ref(MyTerm.makeEnv(_ => Math.random() *. 2.0 -. 0.5))
  while n.contents >= 0 && !shouldBreak.contents {
    n := n.contents - 1
    let result = MyTerm.eval(loss, currParameter.contents)
    let nextParameter = map2(currParameter.contents, result.derivative, (p, dp) => p -. dp *. alpha)

    // when the derivative is almost 0
    if dotproduct(result.derivative, result.derivative) < Float.Constants.epsilon {
      shouldBreak := true
    } else {
      currParameter := nextParameter
    }
  }
}

learn(100, dataset)
