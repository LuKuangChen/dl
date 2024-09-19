// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Caml_obj from "rescript/lib/es6/caml_obj.js";
import * as Utilities from "./Utilities.res.mjs";
import * as Core__Array from "@rescript/core/src/Core__Array.res.mjs";
import * as Core__Option from "@rescript/core/src/Core__Option.res.mjs";

function Term(Env) {
  var ref = function (i) {
    return function (env) {
      return {
              output: Env.get(env, i),
              derivative: Env.update(Env.constant(0.0), i, 1.0)
            };
    };
  };
  var c = function (n) {
    return function (_env) {
      return {
              output: n,
              derivative: Env.constant(0.0)
            };
    };
  };
  var spy = function (x, name) {
    return function (env) {
      var x$1 = x(env);
      console.log(name + " =", x$1.output);
      return x$1;
    };
  };
  var $plus = function (x, y) {
    return function (env) {
      var x$1 = x(env);
      var y$1 = y(env);
      return {
              output: x$1.output + y$1.output,
              derivative: Env.vadd(x$1.derivative, y$1.derivative)
            };
    };
  };
  var $neg = function (x, y) {
    return function (env) {
      var x$1 = x(env);
      var y$1 = y(env);
      return {
              output: x$1.output - y$1.output,
              derivative: Env.vsub(x$1.derivative, y$1.derivative)
            };
    };
  };
  var $tilde$neg = function (x) {
    return $neg((function (_env) {
                  return {
                          output: 0.0,
                          derivative: Env.constant(0.0)
                        };
                }), x);
  };
  var $star = function (x, y) {
    return function (env) {
      var x$1 = x(env);
      var y$1 = y(env);
      return {
              output: x$1.output * y$1.output,
              derivative: Env.vadd(Env.emul(x$1.derivative, y$1.output), Env.emul(y$1.derivative, x$1.output))
            };
    };
  };
  var pow = function (x, n) {
    return function (env) {
      var x$1 = x(env);
      return {
              output: Math.pow(x$1.output, n),
              derivative: Env.emul(x$1.derivative, n * Math.pow(x$1.output, n - 1.0))
            };
    };
  };
  var exp = function (x) {
    return function (env) {
      var x$1 = x(env);
      return {
              output: Math.exp(x$1.output),
              derivative: Env.emul(x$1.derivative, Math.exp(x$1.output))
            };
    };
  };
  var log = function (x) {
    return function (env) {
      var x$1 = x(env);
      return {
              output: Math.log(x$1.output),
              derivative: Env.emul(x$1.derivative, 1.0 / x$1.output)
            };
    };
  };
  var $slash = function (x, y) {
    return $star(x, pow(y, -1.0));
  };
  var sigmoid = function (x) {
    return $slash((function (_env) {
                  return {
                          output: 1.0,
                          derivative: Env.constant(0.0)
                        };
                }), $plus((function (_env) {
                      return {
                              output: 1.0,
                              derivative: Env.constant(0.0)
                            };
                    }), exp($tilde$neg(x))));
  };
  var reLU = function (x) {
    return function (env) {
      var x$1 = x(env);
      if (x$1.output >= 0.0) {
        return x$1;
      } else {
        return {
                output: 0.0,
                derivative: Env.constant(0.0)
              };
      }
    };
  };
  var leakyReLU = function (x) {
    return function (env) {
      if (x(env).output >= 0.0) {
        return x(env);
      } else {
        return $star((function (_env) {
                        return {
                                output: 0.1,
                                derivative: Env.constant(0.0)
                              };
                      }), x)(env);
      }
    };
  };
  var dotproduct = function (v1, v2) {
    return Core__Array.reduce(Utilities.map2($star, v1, v2), (function (_env) {
                  return {
                          output: 0.0,
                          derivative: Env.constant(0.0)
                        };
                }), $plus);
  };
  return {
          ref: ref,
          c: c,
          spy: spy,
          $plus: $plus,
          $neg: $neg,
          $tilde$neg: $tilde$neg,
          $star: $star,
          pow: pow,
          exp: exp,
          log: log,
          $slash: $slash,
          sigmoid: sigmoid,
          reLU: reLU,
          leakyReLU: leakyReLU,
          dotproduct: dotproduct
        };
}

function ArrayEnv(Length) {
  var constant = function (n) {
    return Core__Array.make(Length.length, n);
  };
  var update = function (t, i, m) {
    return t.map(function (n, j) {
                if (i === j) {
                  return m;
                } else {
                  return n;
                }
              });
  };
  var get = function (t, i) {
    return Core__Option.getExn(t[i], undefined);
  };
  var vadd = function (x, y) {
    return Utilities.map2((function (x, y) {
                  return x + y;
                }), x, y);
  };
  var vsub = function (x, y) {
    return Utilities.map2((function (x, y) {
                  return x - y;
                }), x, y);
  };
  var vmul = function (x, y) {
    return Utilities.map2((function (x, y) {
                  return x * y;
                }), x, y);
  };
  var vdiv = function (x, y) {
    return Utilities.map2((function (x, y) {
                  return x / y;
                }), x, y);
  };
  var eadd = function (x, y) {
    return x.map(function (x) {
                return x + y;
              });
  };
  var esub = function (x, y) {
    return x.map(function (x) {
                return x - y;
              });
  };
  var emul = function (x, y) {
    return x.map(function (x) {
                return x * y;
              });
  };
  var ediv = function (x, y) {
    return x.map(function (x) {
                return x / y;
              });
  };
  return {
          constant: constant,
          update: update,
          get: get,
          vadd: vadd,
          vsub: vsub,
          vmul: vmul,
          vdiv: vdiv,
          eadd: eadd,
          esub: esub,
          emul: emul,
          ediv: ediv
        };
}

function constant(n) {
  return Core__Array.make(2, n);
}

function update(t, i, m) {
  return t.map(function (n, j) {
              if (i === j) {
                return m;
              } else {
                return n;
              }
            });
}

function get(t, i) {
  return Core__Option.getExn(t[i], undefined);
}

function vadd(x, y) {
  return Utilities.map2((function (x, y) {
                return x + y;
              }), x, y);
}

function vsub(x, y) {
  return Utilities.map2((function (x, y) {
                return x - y;
              }), x, y);
}

function emul(x, y) {
  return x.map(function (x) {
              return x * y;
            });
}

function ref(i) {
  return function (env) {
    return {
            output: get(env, i),
            derivative: update(constant(0.0), i, 1.0)
          };
  };
}

function $plus(x, y) {
  return function (env) {
    var x$1 = x(env);
    var y$1 = y(env);
    return {
            output: x$1.output + y$1.output,
            derivative: vadd(x$1.derivative, y$1.derivative)
          };
  };
}

function $neg(x, y) {
  return function (env) {
    var x$1 = x(env);
    var y$1 = y(env);
    return {
            output: x$1.output - y$1.output,
            derivative: vsub(x$1.derivative, y$1.derivative)
          };
  };
}

function $tilde$neg(x) {
  return $neg((function (_env) {
                return {
                        output: 0.0,
                        derivative: constant(0.0)
                      };
              }), x);
}

function $star(x, y) {
  return function (env) {
    var x$1 = x(env);
    var y$1 = y(env);
    return {
            output: x$1.output * y$1.output,
            derivative: vadd(emul(x$1.derivative, y$1.output), emul(y$1.derivative, x$1.output))
          };
  };
}

function pow(x, n) {
  return function (env) {
    var x$1 = x(env);
    return {
            output: Math.pow(x$1.output, n),
            derivative: emul(x$1.derivative, n * Math.pow(x$1.output, n - 1.0))
          };
  };
}

function exp(x) {
  return function (env) {
    var x$1 = x(env);
    return {
            output: Math.exp(x$1.output),
            derivative: emul(x$1.derivative, Math.exp(x$1.output))
          };
  };
}

function log(x) {
  return function (env) {
    var x$1 = x(env);
    return {
            output: Math.log(x$1.output),
            derivative: emul(x$1.derivative, 1.0 / x$1.output)
          };
  };
}

function $slash(x, y) {
  return $star(x, pow(y, -1.0));
}

function dotproduct(v1, v2) {
  return Core__Array.reduce(Utilities.map2($star, v1, v2), (function (_env) {
                return {
                        output: 0.0,
                        derivative: constant(0.0)
                      };
              }), $plus);
}

var x = ref(0);

var y = ref(1);

var result = x([
      3.0,
      5.0
    ]);

if (!Caml_obj.equal(result, {
        output: 3.0,
        derivative: [
          1.0,
          0.0
        ]
      })) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "AutoDiff.res",
          179,
          4
        ],
        Error: new Error()
      };
}

var result$1 = y([
      3.0,
      5.0
    ]);

if (!Caml_obj.equal(result$1, {
        output: 5.0,
        derivative: [
          0.0,
          1.0
        ]
      })) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "AutoDiff.res",
          189,
          4
        ],
        Error: new Error()
      };
}

var z = $plus(x, y);

var result$2 = z([
      3.0,
      5.0
    ]);

if (!Caml_obj.equal(result$2, {
        output: 8.0,
        derivative: [
          1.0,
          1.0
        ]
      })) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "AutoDiff.res",
          199,
          4
        ],
        Error: new Error()
      };
}

var z$1 = $neg(x, y);

var result$3 = z$1([
      3.0,
      5.0
    ]);

if (!Caml_obj.equal(result$3, {
        output: -2.0,
        derivative: [
          1.0,
          -1.0
        ]
      })) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "AutoDiff.res",
          209,
          4
        ],
        Error: new Error()
      };
}

var z$2 = $tilde$neg(y);

var result$4 = z$2([
      3.0,
      5.0
    ]);

if (!Caml_obj.equal(result$4, {
        output: -5.0,
        derivative: [
          0.0,
          -1.0
        ]
      })) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "AutoDiff.res",
          219,
          4
        ],
        Error: new Error()
      };
}

var z$3 = $star(x, y);

var result$5 = z$3([
      3.0,
      5.0
    ]);

if (!Caml_obj.equal(result$5, {
        output: 15.0,
        derivative: [
          5.0,
          3.0
        ]
      })) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "AutoDiff.res",
          229,
          4
        ],
        Error: new Error()
      };
}

var z$4 = $slash(x, y);

var result$6 = z$4([
      3.0,
      5.0
    ]);

if (!Caml_obj.equal(result$6, {
        output: 0.6000000000000001,
        derivative: [
          0.2,
          -3.0 / 25.0
        ]
      })) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "AutoDiff.res",
          239,
          4
        ],
        Error: new Error()
      };
}

var z$5 = pow(x, 5.0);

z$5([
      Math.E,
      5.0
    ]);

var z$6 = exp(x);

var result$7 = z$6([
      3.0,
      5.0
    ]);

if (!Caml_obj.equal(result$7, {
        output: Math.exp(3.0),
        derivative: [
          Math.exp(3.0),
          0.0
        ]
      })) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "AutoDiff.res",
          268,
          4
        ],
        Error: new Error()
      };
}

var z$7 = log(x);

var result$8 = z$7([
      3.0,
      5.0
    ]);

if (!Caml_obj.equal(result$8, {
        output: Math.log(3.0),
        derivative: [
          1.0 / 3.0,
          0.0
        ]
      })) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "AutoDiff.res",
          278,
          4
        ],
        Error: new Error()
      };
}

var z$8 = dotproduct([
      (function (_env) {
          return {
                  output: 1.0,
                  derivative: constant(0.0)
                };
        }),
      x,
      y
    ], [
      (function (_env) {
          return {
                  output: 1.0,
                  derivative: constant(0.0)
                };
        }),
      (function (_env) {
          return {
                  output: 2.0,
                  derivative: constant(0.0)
                };
        }),
      (function (_env) {
          return {
                  output: 4.0,
                  derivative: constant(0.0)
                };
        })
    ]);

var result$9 = z$8([
      3.0,
      5.0
    ]);

if (!Caml_obj.equal(result$9, {
        output: 27.0,
        derivative: [
          2.0,
          4.0
        ]
      })) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "AutoDiff.res",
          288,
          4
        ],
        Error: new Error()
      };
}

var z$9 = $plus(pow(x, 2.0), $star(x, y));

var result$10 = z$9([
      3.0,
      5.0
    ]);

if (result$10.output !== 24.0) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "AutoDiff.res",
          296,
          2
        ],
        Error: new Error()
      };
}

if (!Caml_obj.equal(result$10.derivative, [
        11.0,
        3.0
      ])) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "AutoDiff.res",
          297,
          2
        ],
        Error: new Error()
      };
}

export {
  Term ,
  ArrayEnv ,
}
/* x Not a pure module */
