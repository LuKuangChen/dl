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

let bound = (n: float) => {
  if n == Float.Constants.negativeInfinity {
    -.Float.Constants.maxValue
  } else if n == Float.Constants.positiveInfinity {
    Float.Constants.maxValue
  } else {
    n
  }
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

module BoundMath = {
  let max = (x, y) => if x >= y { x } else { y }
  let min = (x, y) => if x <= y { x } else { y }
  let ensurePos = x => max(x, Float.Constants.epsilon)
  let pow = (x, ~exp) => bound(Math.pow(x, ~exp)) -> ensurePos
  let exp = (x) => bound(Math.exp(x)) -> ensurePos
  let log = (x) => bound(Math.log(x->ensurePos))
}