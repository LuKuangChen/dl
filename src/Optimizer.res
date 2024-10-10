open Utilities
open! AutoDiff

type optimizer = (~value: array<float>, ~gradient: array<float>) => array<float>

let learn = (~eval, ~model, ~loss, ~dataset, ~optimizer, ~iteration, ~init) => {
  let rec loop = (~iteration, ~init, ~currLoss) =>
    if iteration == 0 {
      init
    } else {
      let {output: nextLoss, derivative} = eval(
        loss(
          dataset->Array.map(datum => model(datum.input)),
          dataset->Array.map(datum => datum.output),
        ),
        init
      )
      if Math.pow(currLoss -. nextLoss, ~exp=2.0) < Float.Constants.epsilon {
        init
      } else {
        let iteration = iteration - 1
        let init = optimizer(~value=init, ~gradient=derivative)
        let currLoss = nextLoss
        loop(~iteration, ~init, ~currLoss)
      }
    }
    loop(~iteration, ~init, ~currLoss=Float.Constants.positiveInfinity)
}

let alpha = (~alpha=0.1): optimizer => {
  (~value, ~gradient) => {
    vsub(value, emul(gradient, alpha))
  }
}

let adam = (~alpha=0.001, ~beta1=0.9, ~beta2=0.999, ~epsilon=1e-8, ~length): optimizer => {
  // https://doi.org/10.48550/arXiv.1412.6980
  assert(alpha >= 0.0)
  assert(0.0 <= beta1 && beta1 < 1.0)
  assert(0.0 <= beta2 && beta2 < 1.0)
  // 1st moment
  let m = ref(Array.make(~length, 0.0))
  // 2nd moment
  let v = ref(Array.make(~length, 0.0))
  // timestep
  let t = ref(0)
  (~value, ~gradient) => {
    t := t.contents + 1
    m := {
        let m = m.contents
        vadd(emul(m, beta1), emul(gradient, 1.0 -. beta1))
      }

    v := {
        let v = v.contents
        vadd(emul(v, beta2), emul(Array.map(gradient, g => Math.pow(g, ~exp=2.0)), 1.0 -. beta2))
      }

    let m_hat = ediv(m.contents, 1.0 -. Math.pow(beta1, ~exp=Int.toFloat(t.contents)))
    let v_hat = ediv(v.contents, 1.0 -. Math.pow(beta2, ~exp=Int.toFloat(t.contents)))
    vsub(value, emul(vdiv(m_hat, eadd(Array.map(v_hat, Math.sqrt), epsilon)), alpha))
  }
}
