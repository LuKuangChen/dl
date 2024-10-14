module type DataSchema = {
  type input
  type output
  type datum = {input: input, output: output}
}

module type Model = {
  module DataSchema: DataSchema
  module Term: AutoDiff.Term
  type distribution
  let f: DataSchema.input => distribution
  let mode: (distribution, AutoDiff.env) => DataSchema.output
  let loss: (distribution, DataSchema.output) => Term.term
}

type optimizer = (~value: array<float>, ~gradient: array<float>) => array<float>

module Learn = (Model: Model) => {
  open Model
  open DataSchema
  let learn = (dataset: array<datum>, ~optimizer: optimizer, ~iteration=100): AutoDiff.env => {
    let loss =
      dataset
      ->Array.map(({input, output}) => {
        loss(f(input), output)
      })
      ->Array.reduce(Term.c(0.0), Term.\"+")
    let init = Term.initEnv
    let rec loop = (~iteration, ~env, ~currLoss) =>
      if iteration == 0 {
        env
      } else {
        let {output: nextLoss, derivative} = Term.eval(loss, env)
        if Math.pow(currLoss -. nextLoss, ~exp=2.0) < Float.Constants.epsilon {
          env
        } else {
          let iteration = iteration - 1
          let env = optimizer(~value=env, ~gradient=derivative)
          let currLoss = nextLoss
          loop(~iteration, ~env, ~currLoss)
        }
      }
    loop(~iteration, ~env=init, ~currLoss=Float.Constants.positiveInfinity)
  }
}
