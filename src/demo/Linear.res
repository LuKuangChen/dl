open AutoDiff

module MyTerm = MakeTerm()
module MyExtraOps = ExtraOperators(MyTerm)

let linearModel = (nInputs: int, ~iteration=100) => {
  // declare initializer
  let init = () => Math.random() *. 2.0 -. 0.5
  let optimizer = Optimizer.adam(~length=nInputs + 1)
  let (model, loss, eval, init) = {
    open! MyTerm
    open! MyExtraOps
    // declare parameters
    let weights = claimMany(nInputs, ~init)
    let bias = claim(~init)
    // define the model
    let model = xs => {
      dotproduct(weights, xs) + bias
    }
    // define the loss/utility function
    let loss = (guessed, actual) => {
      let actual = actual->Array.map(c)
      (guessed -. actual)
      ->Array.map(d => pow(d, 2.0))
      ->mean
    }
    (model, loss, eval, initEnv)
  }
  dataset => {
    Optimizer.learn(~eval, ~model, ~loss, ~dataset, ~optimizer, ~iteration, ~init)
  }
}
