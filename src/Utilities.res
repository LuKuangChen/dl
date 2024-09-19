let map2 = (f, ns, ms) => {
  assert(Array.length(ns) == Array.length(ms))
  Array.mapWithIndex(ns, (n, i) => {
    let m = ms[i]->Option.getExn
    f(n, m)
  })
}

let dotproduct = (v1, v2) => {
  Array.reduce(map2(\"*.", v1, v2), 0.0, \"+.")
}

let buildArray = (length, f) => {
  Array.make(~length, 0)->Array.mapWithIndex((_, i) => {
    f(i)
  })
}

let sigmoid = (x: float) => {
  1.0 /. (1.0 +. Math.exp(-.x))
}

let d_sigmoid = (x: float) => {
  sigmoid(x) *. (1.0 -. sigmoid(x))
}

let sum = ns => {
  ns->Array.reduce(0., (a, b) => a +. b)
}

let nonnegative = (i: int) => {
  0 <= i
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
