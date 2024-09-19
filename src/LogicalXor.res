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

let parameterCount = 9

module MyEnv = ArrayEnv({
  let length = parameterCount
})

module MyTerm = Term(MyEnv)

let makeInitParameters = (): MyEnv.t =>
  buildArray(parameterCount, _ => (Math.random() -. 0.5) *. 1.0)

let loss = dataset => {
  open! MyTerm
  let parameters = buildArray(parameterCount, ref)
  let w11 = Array.slice(parameters, ~start=0, ~end=3)
  let w12 = Array.slice(parameters, ~start=3, ~end=6)
  let w2 = Array.slice(parameters, ~start=6, ~end=9)
  spy(
    dataset
    ->Array.map(datum => {
      let {input, output} = datum
      let (x1, x2) = input
      let x = spy(c(inject(x1)), "x")
      let y = spy(c(inject(x2)), "y")
      let h11 = sigmoid(dotproduct([c(1.0), x, y], w11))
      let h12 = sigmoid(dotproduct([c(1.0), x, y], w12))
      let h2 = sigmoid(dotproduct([c(1.0), h11, h12], w2))
      let pred = spy(
        h2,
        `Pr`,
      )
      spy(
        -log(
          if output {
            pred
          } else {
            c(1.0) - pred
          },
        ),
        "loss",
      )
    })
    ->Array.reduce(c(0.0), \"+"),
    "TOTAL LOSS",
  )
}

let closeEnough = (env1, env2) => {
  sum(map2((x, y) => Math.pow(x -. y, ~exp=2.0), env1, env2)) <= Float.Constants.epsilon
}

let learn = (iteration: int, dataset) => {
  assert(iteration >= 0)
  let loss = loss(dataset)
  let n = ref(iteration)
  let shouldBreak = ref(false)
  let currParameter = ref(makeInitParameters())
  while n.contents >= 0 && !shouldBreak.contents {
    n := n.contents - 1
    Console.log(currParameter.contents)
    let result = loss(currParameter.contents)
    let nextParameter = map2((p, dp) => p -. dp *. alpha, currParameter.contents, result.derivative)

    // when the derivative is almost 0
    if dotproduct(result.derivative, result.derivative) < Float.Constants.epsilon {
      shouldBreak := true
    } else {
      currParameter := nextParameter
    }
  }
}

learn(1000, dataset)
