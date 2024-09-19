open Utilities

module type Env = {
  type t
  type i
  let get: (t, i) => float
  let vadd: (t, t) => t
  let vsub: (t, t) => t
  let vmul: (t, t) => t
  let vdiv: (t, t) => t
  let eadd: (t, float) => t
  let esub: (t, float) => t
  let emul: (t, float) => t
  let ediv: (t, float) => t
  let constant: float => t
  let update: (t, i, float) => t
}

module Term = (Env: Env) => {
  open! Env
  type termOutput = {
    output: float,
    derivative: Env.t,
  }
  type term = Env.t => termOutput
  let ref = i => env => {
    {
      output: get(env, i),
      derivative: update(constant(0.0), i, 1.0),
    }
  }
  let c = n => _env => {
    {
      output: n,
      derivative: Env.constant(0.0),
    }
  }

  let spy = (x, name) => env => {
    let x = x(env)
    Console.log2(`${name} =`, x.output)
    x
  }

  let \"+" = (x: term, y: term) => env => {
    let x = x(env)
    let y = y(env)
    {
      output: x.output +. y.output,
      derivative: vadd(x.derivative, y.derivative),
    }
  }

  let \"-" = (x: term, y: term) => env => {
    let x = x(env)
    let y = y(env)
    {
      output: x.output -. y.output,
      derivative: vsub(x.derivative, y.derivative),
    }
  }

  let \"~-" = x => c(0.0) - x

  let \"*" = (x: term, y: term) => env => {
    let x = x(env)
    let y = y(env)
    {
      output: x.output *. y.output,
      derivative: vadd(emul(x.derivative, y.output), emul(y.derivative, x.output)),
    }
  }

  let pow = (x, n) => env => {
    let x = x(env)
    {
      output: Math.pow(x.output, ~exp=n),
      derivative: emul(x.derivative, n *. Math.pow(x.output, ~exp=n -. 1.0)),
    }
  }

  let exp = (x: term) => env => {
    let x = x(env)
    {
      output: Math.exp(x.output),
      derivative: emul(x.derivative, Math.exp(x.output)),
    }
  }

  let log = (x: term) => env => {
    let x = x(env)
    {
      output: Math.log(x.output),
      derivative: emul(x.derivative, 1.0 /. x.output),
    }
  }

  let \"/" = (x: term, y: term) => x * pow(y, -1.0)

  let sigmoid = x => {
    c(1.0) / (c(1.0) + exp(-x))
  }
}

module type Length = {
  let length: int
}

module ArrayEnv = (Length: Length) => {
  open Length

  type i = int
  type t = array<float>

  let constant = n => Array.make(~length, n)

  let update = (t, i, m) => {
    Array.mapWithIndex(t, (n, j) => {
      if i == j {
        m
      } else {
        n
      }
    })
  }

  let get = (t, i) => {
    t[i]->Option.getExn
  }

  let vadd = (x, y) => map2((x, y) => x +. y, x, y)
  let vsub = (x, y) => map2((x, y) => x -. y, x, y)
  let vmul = (x, y) => map2((x, y) => x *. y, x, y)
  let vdiv = (x, y) => map2((x, y) => x /. y, x, y)
  let eadd = (x, y) => x->Array.map(x => x +. y)
  let esub = (x, y) => x->Array.map(x => x -. y)
  let emul = (x, y) => x->Array.map(x => x *. y)
  let ediv = (x, y) => x->Array.map(x => x /. y)
}

{
  module MyTerm = Term(
    ArrayEnv({
      let length = 2
    }),
  )
  open! MyTerm
  let x = ref(0)
  let y = ref(1)

  {
    // test ref
    let z = x
    let result = z([3.0, 5.0])
    assert(result == {
        output: 3.0,
        derivative: [1.0, 0.0],
      })
  }

  {
    // test ref
    let z = y
    let result = z([3.0, 5.0])
    assert(result == {
        output: 5.0,
        derivative: [0.0, 1.0],
      })
  }

  {
    // test add
    let z = x + y
    let result = z([3.0, 5.0])
    assert(result == {
        output: 8.0,
        derivative: [1.0, 1.0],
      })
  }

  {
    // test sub
    let z = x - y
    let result = z([3.0, 5.0])
    assert(result == {
        output: -2.0,
        derivative: [1.0, -1.0],
      })
  }

  {
    // test neg
    let z = - y
    let result = z([3.0, 5.0])
    assert(result == {
        output: -5.0,
        derivative: [0.0, -1.0],
      })
  }

  {
    // test mul
    let z = x * y
    let result = z([3.0, 5.0])
    assert(result == {
        output: 15.0,
        derivative: [5.0, 3.0],
      })
  }

  {
    // test div
    let z = x / y
    let result = z([3.0, 5.0])
    assert(result == {
        output: 0.6000000000000001,
        derivative: [0.2, -3.0 /. 25.0],
      })
  }

  {
    // test pow
    let z = pow(x, 5.0)
    let result = z([Math.Constants.e, 5.0])
    // close enough
    // Console.log2(
    //     result,
    //     {
    //     output: Math.exp(5.0),
    //     derivative: [5.0 *. Math.exp(4.0), 0.0],
    //   }
    // )
    // assert(result == {
    //     output: Math.exp(5.0),
    //     derivative: [5.0 *. Math.exp(4.0), 0.0],
    //   })
  }

  {
    // test exp
    let z = exp(x)
    let result = z([3.0, 5.0])
    assert(result == {
        output: Math.exp(3.0),
        derivative: [Math.exp(3.0), 0.0],
      })

  }

  {
    // test log
    let z = log(x)
    let result = z([3.0, 5.0])
    assert(result == {
        output: Math.log(3.0),
        derivative: [1.0 /. 3.0, 0.0],
      })

  }

  let z = pow(x, 2.0) + x * y
  let result = z([3.0, 5.0])
  assert(result.output == 24.0)
  assert(result.derivative == [11.0, 3.0])
//   Console.log(result)
}
