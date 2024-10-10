open AutoDiff

module MyTerm = MakeTerm()
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
      derivative: [0.2, -0.12],
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
      derivative: [272.9907501657212, 0.0],
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
      derivative: [0.3333333333333333, 0.0],
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

checkEq(
  "ReLU-1",
  eval(reLU(x), [3.0, 5.0]),
  {
    output: 3.0,
    derivative: [1.0, 0.0],
  },
)

checkEq(
  "ReLU-2",
  eval(reLU(x - y), [3.0, 5.0]),
  {
    output: 0.0,
    derivative: [0.0, 0.0],
  },
)

checkEq(
  "leakyReLU-1",
  eval(leakyReLU(x), [3.0, 5.0]),
  {
    output: 3.0,
    derivative: [1.0, 0.0],
  },
)

checkEq(
  "leakyReLU-2",
  eval(leakyReLU(x - y), [3.0, 5.0]),
  {
    output: -0.2,
    derivative: [0.1, -0.1],
  },
)

assert(evalCond(x == y, [4.0, 4.0]))
assert(evalCond(!(x == y), [3.0, 5.0]))
assert(evalCond(x < y, [3.0, 5.0]))
assert(evalCond(!(x < y), [5.0, 5.0]))
assert(evalCond(x == y || x < y, [3.0, 5.0]))
assert(evalCond(!(x == y && x < y), [3.0, 5.0]))
checkEq(
  "ifte-t",
  eval(ifte(c(1.0) < c(2.0), x, y), [3.0, 5.0]),
  {
    output: 3.0,
    derivative: [1.0, 0.0],
  },
)
checkEq(
  "ifte-f",
  eval(ifte(c(9.0) < c(2.0), x, y), [3.0, 5.0]),
  {
    output: 5.0,
    derivative: [0.0, 1.0],
  },
)
