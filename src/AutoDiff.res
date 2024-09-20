open Utilities

module type Nat = {
  let n: int
}

module type Term = {
  type env = array<float>
  let makeEnv: (unit => float) => env
  type cond
  type t
  type termMeaning = {
    output: float,
    derivative: env,
  }
  let spy: (t, string) => t
  let eval: (t, env) => termMeaning
  let claim: unit => t
  let claimMany: int => array<t>
  let c: float => t
  let \"+": (t, t) => t
  let \"*": (t, t) => t
  let \"<": (t, t) => cond
  let \"=": (t, t) => cond
  let not: cond => cond
  let \"&&": (cond, cond) => cond
  let \"||": (cond, cond) => cond
  let pow: (t, float) => t
  let exp: t => t
  let log: t => t
  let ifte: (cond, t, t) => t
  let checkEq: (string, termMeaning, termMeaning) => unit
}

module MakeTerm = (Nat: Nat): Term => {
  type env = array<float>
  type termMeaning = {
    output: float,
    derivative: env,
  }
  type t = Var(int) | Term(env => termMeaning)
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
    | Term(t) => t(env)
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
        output: Math.pow(x.output, ~exp=n),
        derivative: emul(x.derivative, n *. Math.pow(x.output, ~exp=n -. 1.0)),
      }
    },
  )
  let exp = x => Term(
    env => {
      let x = eval(x, env)
      {
        output: Math.exp(x.output),
        derivative: emul(x.derivative, Math.exp(x.output)),
      }
    },
  )

  let log = x => Term(
    env => {
      let x = eval(x, env)
      {
        output: Math.log(x.output),
        derivative: emul(x.derivative, 1.0 /. x.output),
      }
    },
  )

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
}

module ExtraOperators = (Term: Term) => {
  open! Term
  type term = Term.t

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

  let reLU = x => ifte(x > c(0.0), x, c(0.0))
  let leakyReLU = x => ifte(x > c(0.0), x, c(0.1) * x)

  let dotproduct = (v1, v2) => {
    Array.reduce(map2(v1, v2, \"*"), c(0.0), \"+")
  }
}

{
  module MyTerm = MakeTerm({
    let n = 2
  })
  open! MyTerm
  module Op = ExtraOperators(MyTerm)
  open! Op
  let x = claim()
  let y = claim()

  {
    // test ref
    let result = eval(x, [3.0, 5.0])
    checkEq(
      "refx",
      result,
      {
        output: 3.0,
        derivative: [1.0, 0.0],
      },
    )
  }

  {
    // test ref
    let result = eval(y, [3.0, 5.0])
    checkEq(
      "refy",
      result,
      {
        output: 5.0,
        derivative: [0.0, 1.0],
      },
    )
  }

  {
    // test add
    let result = eval(x + y, [3.0, 5.0])
    checkEq(
      "add",
      result,
      {
        output: 8.0,
        derivative: [1.0, 1.0],
      },
    )
  }

  {
    // test sub
    let result = eval(x - y, [3.0, 5.0])
    checkEq(
      "sub",
      result,
      {
        output: -2.0,
        derivative: [1.0, -1.0],
      },
    )
  }

  {
    // test neg
    let result = eval(-y, [3.0, 5.0])
    checkEq(
      "neg",
      result,
      {
        output: -5.0,
        derivative: [0.0, -1.0],
      },
    )
  }

  {
    // test mul
    let result = eval(x * y, [3.0, 5.0])
    checkEq(
      "mul",
      result,
      {
        output: 15.0,
        derivative: [5.0, 3.0],
      },
    )
  }

  {
    // test div
    let result = eval(x / y, [3.0, 5.0])
    checkEq(
      "div",
      result,
      {
        output: 0.6000000000000001,
        derivative: [0.2, -3.0 /. 25.0],
      },
    )
  }

  {
    // test pow
    let result = eval(pow(x, 5.0), [Math.Constants.e, 5.0])
    checkEq(
      "pow",
      result,
      {
        output: Math.exp(5.0),
        derivative: [5.0 *. Math.exp(4.0), 0.0],
      },
    )
  }

  {
    // test exp
    let z = exp(x)
    let result = eval(z, [3.0, 5.0])
    checkEq(
      "exp",
      result,
      {
        output: Math.exp(3.0),
        derivative: [Math.exp(3.0), 0.0],
      },
    )
  }

  {
    // test log
    let z = log(x)
    let result = eval(z, [3.0, 5.0])
    checkEq(
      "log",
      result,
      {
        output: Math.log(3.0),
        derivative: [1.0 /. 3.0, 0.0],
      },
    )
  }

  // test dotproduct
  let z = dotproduct([c(1.0), x, y], [c(1.0), c(2.0), c(4.0)])
  let result = eval(z, [3.0, 5.0])
  checkEq(
    "dotproduct",
    result,
    {
      output: 27.0,
      derivative: [2.0, 4.0],
    },
  )
}
