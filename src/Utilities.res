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
