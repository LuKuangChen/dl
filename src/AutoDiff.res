open Utilities

module type Nat = {
  let n: int
}

type env = array<float>
type termMeaning = {
  output: float,
  derivative: env,
}


module type Term = {
  type cond
  type term
  let claim: (~init:unit=>float=?) => term
  let claimMany: (int, ~init:unit=>float=?) => array<term>
  let c: float => term
  let \"+": (term, term) => term
  let \"*": (term, term) => term
  let \"<": (term, term) => cond
  let \"=": (term, term) => cond
  let not: cond => cond
  let \"&&": (cond, cond) => cond
  let \"||": (cond, cond) => cond
  let pow: (term, float) => term
  let exp: term => term
  let log: term => term
  let ifte: (cond, term, term) => term
  let track: (term, string) => term

  let initEnv: array<float>
  let eval: (term, env) => termMeaning
  let evalCond: (cond, env) => bool
  let checkEq: (string, termMeaning, termMeaning) => unit

  let makeEnv: (int => float) => env
  let updateEnv: (env, (int, float) => float) => env
}

module MakeTerm = (): Term => {
  type term = Var(int) | Term(env => termMeaning)
  type cond = env => bool

  let nVariable = ref(0)
  let nVariableLocked = ref(false)

  let initEnv = []

  let claim = (~init=()=>0.0) => {
    let i = nVariable.contents
    if !nVariableLocked.contents {
      nVariable := nVariable.contents + 1
      Array.push(initEnv, init())
      Var(i)
    } else {
      failwith("Trying to claim new variables after the world has been locked.")
    }
  }
  let rec claimMany = (n,~init=()=>0.0) => {
    if n == 0 {
      []
    } else {
      [claim(~init), ...claimMany(n - 1, ~init)]
    }
  }

  let eval = (term, env) => {
    nVariableLocked := true
    switch term {
    | Var(i) => {
        output: env[i]->Option.getExn,
        derivative: {
          let d = Array.make(~length=nVariable.contents, 0.0)
          d[i] = 1.0
          d
        },
      }
    | Term(term) => term(env)
    }
  }
  let evalCond = (cond, env) => {
    nVariableLocked := true
    cond(env)
  }

  let checkEq = (test, m1, m2) => {
    let checkCloseEnough = (x, y, s) => {
      if !closeEnough(x, y) {
        Console.log2("fail check:", s)
        Console.log2("- received:", x)
        Console.log2("- expected:", y)
      }
    }
    checkCloseEnough(m1.output, m2.output, `${test}-output`)
    map2(m1.derivative, m2.derivative, (d1, d2) => {
      checkCloseEnough(d1, d2, `${test}-derivative`)
    })->ignore
  }

  let track = (x, name) => Term(
    env => {
      let x = eval(x, env)
      Console.log2(`${name} =`, x.output)
      x
    },
  )

  let c = v => Term(
    _env => {
      output: v,
      derivative: Array.make(~length=nVariable.contents, 0.0),
    },
  )

  let \"+" = (x, y) => Term(
    env => {
      let x = eval(x, env)
      let y = eval(y, env)
      {
        output: x.output +. y.output,
        derivative: vadd(x.derivative, y.derivative),
      }
    },
  )

  let \"*" = (x, y) => Term(
    env => {
      let x = eval(x, env)
      let y = eval(y, env)
      {
        output: x.output *. y.output,
        derivative: vadd(emul(x.derivative, y.output), emul(y.derivative, x.output)),
      }
    },
  )

  let pow = (x, n) => Term(
    env => {
      let x = eval(x, env)
      {
        output: CheckedMath.pow(x.output, n),
        derivative: emul(x.derivative, n *. CheckedMath.pow(x.output, n -. 1.0)),
      }
    },
  )
  let exp = x => Term(
    env => {
      let x = eval(x, env)
      {
        output: CheckedMath.exp(x.output),
        derivative: emul(x.derivative, CheckedMath.exp(x.output)),
      }
    },
  )

  let log = x => Term(
    env => {
      let x = eval(x, env)
      {
        output: CheckedMath.log(x.output),
        derivative: emul(x.derivative, 1.0 /. x.output),
      }
    },
  )

  let \"=" = (x, y) => env => {
    let x = eval(x, env)
    let y = eval(y, env)
    x.output == y.output
  }

  let \"<" = (x, y) => env => {
    let x = eval(x, env)
    let y = eval(y, env)
    x.output < y.output
  }

  let ifte = (b, x, y) => {
    Term(
      env => {
        if b(env) {
          eval(x, env)
        } else {
          eval(y, env)
        }
      },
    )
  }

  let \"||" = (a, b) => env => {
    a(env) || b(env)
  }

  let \"&&" = (a, b) => env => {
    a(env) && b(env)
  }

  let not = a => env => {
    !a(env)
  }

  let makeEnv = maker => {
    let ns = Array.make(~length=nVariable.contents, 0.0)
    ns->Array.mapWithIndex((_, i) => maker(i))
  }

  let updateEnv = (env, updater) => {
    env->Array.mapWithIndex((v, i) => updater(i, v))
  }
}

module ExtraOperators = (Term: Term) => {
  open! Term
  type term = Term.term

  let \"~-" = (x: term) => c(-1.0) * x
  let \"-" = (x: term, y: term) => x + -y
  let \"/" = (x: term, y: term) => x * pow(y, -1.0)

  let \"+." = (xs, ys) => map2(xs, ys, \"+")
  let \"-." = (xs, ys) => map2(xs, ys, \"-")
  let \"*." = (xs, ys) => map2(xs, ys, \"*")
  let \"/." = (xs, ys) => map2(xs, ys, \"/")

  let \">" = (x, y) => y < x
  let \"<=" = (x, y) => x < y || x == y
  let \">=" = (x, y) => x > y || x == y
  let \"!=" = (x, y) => !(x == y)

  let sigmoid = x => {
    c(1.0) / (c(1.0) + exp(-x))
  }

  let sum = xs => {
    xs->Array.reduce(c(0.0), (a, b) => a + b)
  }

  let mean = xs => {
    sum(xs) / c(Float.fromInt(Array.length(xs)))
  }

  let max = (x, y) => ifte(x >= y, x, y)
  let min = (x, y) => ifte(x <= y, x, y)
  let reLU = x => max(x, c(0.0))
  let leakyReLU = x => ifte(x > c(0.0), x, c(0.1) * x)
  let reELU = x => ifte(x < c(0.0), exp(x), x)

  let dotproduct = (v1, v2) => {
    Array.reduce(map2(v1, v2, \"*"), c(0.0), \"+")
  }
}
