open Utilities

module type Nat = {
  let n: int
}

module type Term = {
  type env = array<float>
  let makeEnv: (unit => float) => env
  type cond = env => bool
  type term
  type termMeaning = {
    output: float,
    derivative: env,
  }
  let spy: (term, string) => term
  let eval: (term, env) => termMeaning
  let claim: unit => term
  let claimMany: int => array<term>
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
  let checkEq: (string, termMeaning, termMeaning) => unit
}

module MakeTerm = (Nat: Nat): Term => {
  type env = array<float>
  type termMeaning = {
    output: float,
    derivative: env,
  }
  type term = Var(int) | Term(env => termMeaning)
  type cond = env => bool

  let nVariable = Nat.n
  let makeEnv = m => buildArray(nVariable, _ => m())

  let claimed = ref(0)
  let claim = () => {
    let i = claimed.contents
    if i < nVariable {
      claimed := claimed.contents + 1
      Var(i)
    } else {
      failwith("Not enough variables")
    }
  }
  let rec claimMany = n => {
    if n == 0 {
      []
    } else {
      [claim(), ...claimMany(n - 1)]
    }
  }

  let eval = (term, env) => {
    switch term {
    | Var(i) => {
        output: env[i]->Option.getExn,
        derivative: {
          let d = Array.make(~length=nVariable, 0.0)
          d[i] = 1.0
          d
        },
      }
    | Term(term) => term(env)
    }
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

  let spy = (x, name) => Term(
    env => {
      let x = eval(x, env)
      Console.log2(`${name} =`, x.output)
      x
    },
  )

  let c = v => Term(
    _env => {
      output: v,
      derivative: Array.make(~length=nVariable, 0.0),
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
        output: BoundMath.pow(x.output, ~exp=n),
        derivative: emul(x.derivative, n *. BoundMath.pow(x.output, ~exp=n -. 1.0)),
      }
    },
  )
  let exp = x => Term(
    env => {
      let x = eval(x, env)
      {
        output: BoundMath.exp(x.output),
        derivative: emul(x.derivative, BoundMath.exp(x.output)),
      }
    },
  )

  let log = x => Term(
    env => {
      let x = eval(x, env)
      {
        output: BoundMath.log(x.output),
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
}

module ExtraOperators = (Term: Term) => {
  open! Term
  type term = Term.term

  let \"~-" = (x: term) => c(-1.0) * x
  let \"-" = (x: term, y: term) => x + -y
  let \"/" = (x: term, y: term) => x * pow(y, -1.0)

  let \">" = (x, y) => y < x
  let \"<=" = (x, y) => x < y || x == y
  let \">=" = (x, y) => x > y || x == y
  let \"!=" = (x, y) => !(x == y)

  let sigmoid = x => {
    c(1.0) / (c(1.0) + exp(-x))
  }

  let max = (x, y) => ifte(x >= y, x, y)
  let min = (x, y) => ifte(x <= y, x, y)
  let reLU = x => max(x, c(0.0))
  let leakyReLU = x => ifte(x > c(0.0), x, c(0.1) * x)

  let dotproduct = (v1, v2) => {
    Array.reduce(map2(v1, v2, \"*"), c(0.0), \"+")
  }
}
