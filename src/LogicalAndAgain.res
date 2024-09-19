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

module MyEnv = ArrayEnv({
  let length = 3
})

module MyTerm = Term(MyEnv)

let makeInitParameters = (): MyEnv.t => [
    Math.random() -. 0.5,
    Math.random() -. 0.5,
    Math.random() -. 0.5,
]

let loss = dataset => {
  open! MyTerm
  let w0 = ref(0)
  let w1 = ref(1)
  let w2 = ref(2)
  spy(
    dataset
    ->Array.map(datum => {
      let {input, output} = datum
      let (x1, x2) = input
      let x1 = spy(c(inject(x1)), "x1")
      let x2 = spy(c(inject(x2)), "x2")
      let h1 = w0 + w1 * x1 + w2 * x2
      let h2 = spy(c(1.0) / (c(1.0) + exp(-h1)), "sigmoid")
      spy(
        -log(
          if output {
            h2
          } else {
            c(1.0) - h2
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
    if closeEnough(currParameter.contents, nextParameter) {
      shouldBreak := true
    } else {
      currParameter := nextParameter
    }
  }
}

learn(5, dataset)
