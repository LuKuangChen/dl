let buildArray = (length, f) => {
  Array.make(~length, 0.0)->Array.mapWithIndex((_, i) => {
    f(i)
  })
}
let map2 = (ns, ms, f) => {
  assert(Array.length(ns) == Array.length(ms))
  Array.mapWithIndex(ns, (n, i) => {
    let m = ms[i]->Option.getExn
    f(n, m)
  })
}

let jitter = (n: float) => {
  if Math.random() >= 0.5 {
    n -. Float.Constants.epsilon
  } else {
    n +. Float.Constants.epsilon
  }
}

let closeEnough = (n: float, m: float) => {
  Math.pow(n -. m, ~exp=2.0) < Float.Constants.epsilon
}

let vadd = (x, y) => map2(x, y, (x, y) => Pervasives.\"+."(x, y))
let vsub = (x, y) => map2(x, y, (x, y) => Pervasives.\"-."(x, y))
let vmul = (x, y) => map2(x, y, (x, y) => Pervasives.\"*."(x, y))
let vdiv = (x, y) => map2(x, y, (x, y) => Pervasives.\"/."(x, y))
let eadd = (x, y) => x->Array.map(x => Pervasives.\"+."(x, y))
let esub = (x, y) => x->Array.map(x => Pervasives.\"-."(x, y))
let emul = (x, y) => x->Array.map(x => Pervasives.\"*."(x, y))
let ediv = (x, y) => x->Array.map(x => Pervasives.\"/."(x, y))
let dotproduct = (v1, v2) => {
  Array.reduce(map2(v1, v2, \"*."), 0.0, \"+.")
}

let isGoodNumber = x => {
  !(!Float.isFinite(x) || Float.isNaN(x))
}
let checkGoodNumber = (x, message) => {
  if !isGoodNumber(x) {
    panic(`${message} is not a good number: ${Float.toString(x)}`)
  }
}
let checkedOp1 = (name, f) => x => {
  checkGoodNumber(x, `${name}'s argument`)
  let o = f(x)
  checkGoodNumber(o, `${name}'s output (input is ${x->Float.toString})`)
  o
}
let checkedOp2 = (name, f) => (x, y) => {
  checkGoodNumber(x, `${name}'s first argument`)
  checkGoodNumber(y, `${name}'s second argument`)
  let o = f(x, y)
  checkGoodNumber(o, `${name}'s output (inputs are ${x->Float.toString} and ${y->Float.toString})`)
  o
}

module CheckedMath = {
  let max = checkedOp2("max", (x, y) =>
    if x >= y {
      x
    } else {
      y
    }
  )
  let min = checkedOp2("min", (x, y) =>
    if x <= y {
      x
    } else {
      y
    }
  )
  let pow = checkedOp2("pow", (x, exp) => Math.pow(x, ~exp))
  let exp = checkedOp1("exp", Math.exp)
  let log = checkedOp1("log", Math.log)
}
