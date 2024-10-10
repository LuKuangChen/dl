// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Utilities from "./Utilities.res.mjs";
import * as Belt_Array from "rescript/lib/es6/belt_Array.js";
import * as Core__Array from "@rescript/core/src/Core__Array.res.mjs";
import * as PervasivesU from "rescript/lib/es6/pervasivesU.js";
import * as Core__Option from "@rescript/core/src/Core__Option.res.mjs";

function MakeTerm($star) {
  var nVariable = {
    contents: 0
  };
  var nVariableLocked = {
    contents: false
  };
  var initEnv = [];
  var claim = function (initOpt) {
    var init = initOpt !== undefined ? initOpt : (function () {
          return 0.0;
        });
    var i = nVariable.contents;
    if (nVariableLocked.contents) {
      return PervasivesU.failwith("Trying to claim new variables after the world has been locked.");
    } else {
      nVariable.contents = nVariable.contents + 1 | 0;
      initEnv.push(init());
      return {
              TAG: "Var",
              _0: i
            };
    }
  };
  var claimMany = function (n, initOpt) {
    var init = initOpt !== undefined ? initOpt : (function () {
          return 0.0;
        });
    if (n === 0) {
      return [];
    } else {
      return Belt_Array.concatMany([
                  [claim(init)],
                  claimMany(n - 1 | 0, init)
                ]);
    }
  };
  var $$eval = function (term, env) {
    nVariableLocked.contents = true;
    if (term.TAG !== "Var") {
      return term._0(env);
    }
    var i = term._0;
    var d = Core__Array.make(nVariable.contents, 0.0);
    return {
            output: Core__Option.getExn(env[i], undefined),
            derivative: (d[i] = 1.0, d)
          };
  };
  var evalCond = function (cond, env) {
    nVariableLocked.contents = true;
    return cond(env);
  };
  var checkEq = function (test, m1, m2) {
    var checkCloseEnough = function (x, y, s) {
      if (!Utilities.closeEnough(x, y)) {
        console.log("fail check:", s);
        console.log("- received:", x);
        console.log("- expected:", y);
        return ;
      }
      
    };
    checkCloseEnough(m1.output, m2.output, test + "-output");
    Utilities.map2(m1.derivative, m2.derivative, (function (d1, d2) {
            checkCloseEnough(d1, d2, test + "-derivative");
          }));
  };
  var track = function (x, name) {
    return {
            TAG: "Term",
            _0: (function (env) {
                var x$1 = $$eval(x, env);
                console.log(name + " =", x$1.output);
                return x$1;
              })
          };
  };
  var c = function (v) {
    return {
            TAG: "Term",
            _0: (function (_env) {
                return {
                        output: v,
                        derivative: Core__Array.make(nVariable.contents, 0.0)
                      };
              })
          };
  };
  var $plus = function (x, y) {
    return {
            TAG: "Term",
            _0: (function (env) {
                var x$1 = $$eval(x, env);
                var y$1 = $$eval(y, env);
                return {
                        output: x$1.output + y$1.output,
                        derivative: Utilities.vadd(x$1.derivative, y$1.derivative)
                      };
              })
          };
  };
  var $star$1 = function (x, y) {
    return {
            TAG: "Term",
            _0: (function (env) {
                var x$1 = $$eval(x, env);
                var y$1 = $$eval(y, env);
                return {
                        output: x$1.output * y$1.output,
                        derivative: Utilities.vadd(Utilities.emul(x$1.derivative, y$1.output), Utilities.emul(y$1.derivative, x$1.output))
                      };
              })
          };
  };
  var pow = function (x, n) {
    return {
            TAG: "Term",
            _0: (function (env) {
                var x$1 = $$eval(x, env);
                return {
                        output: Utilities.CheckedMath.pow(x$1.output, n),
                        derivative: Utilities.emul(x$1.derivative, n * Utilities.CheckedMath.pow(x$1.output, n - 1.0))
                      };
              })
          };
  };
  var exp = function (x) {
    return {
            TAG: "Term",
            _0: (function (env) {
                var x$1 = $$eval(x, env);
                return {
                        output: Utilities.CheckedMath.exp(x$1.output),
                        derivative: Utilities.emul(x$1.derivative, Utilities.CheckedMath.exp(x$1.output))
                      };
              })
          };
  };
  var log = function (x) {
    return {
            TAG: "Term",
            _0: (function (env) {
                var x$1 = $$eval(x, env);
                return {
                        output: Utilities.CheckedMath.log(x$1.output),
                        derivative: Utilities.emul(x$1.derivative, 1.0 / x$1.output)
                      };
              })
          };
  };
  var $eq = function (x, y) {
    return function (env) {
      var x$1 = $$eval(x, env);
      var y$1 = $$eval(y, env);
      return x$1.output === y$1.output;
    };
  };
  var $less = function (x, y) {
    return function (env) {
      var x$1 = $$eval(x, env);
      var y$1 = $$eval(y, env);
      return x$1.output < y$1.output;
    };
  };
  var ifte = function (b, x, y) {
    return {
            TAG: "Term",
            _0: (function (env) {
                if (b(env)) {
                  return $$eval(x, env);
                } else {
                  return $$eval(y, env);
                }
              })
          };
  };
  var $pipe$pipe = function (a, b) {
    return function (env) {
      if (a(env)) {
        return true;
      } else {
        return b(env);
      }
    };
  };
  var $amp$amp = function (a, b) {
    return function (env) {
      if (a(env)) {
        return b(env);
      } else {
        return false;
      }
    };
  };
  var not = function (a) {
    return function (env) {
      return !a(env);
    };
  };
  var makeEnv = function (maker) {
    var ns = Core__Array.make(nVariable.contents, 0.0);
    return ns.map(function (param, i) {
                return maker(i);
              });
  };
  var updateEnv = function (env, updater) {
    return env.map(function (v, i) {
                return updater(i, v);
              });
  };
  return {
          claim: claim,
          claimMany: claimMany,
          c: c,
          $plus: $plus,
          $star: $star$1,
          $less: $less,
          $eq: $eq,
          not: not,
          $amp$amp: $amp$amp,
          $pipe$pipe: $pipe$pipe,
          pow: pow,
          exp: exp,
          log: log,
          ifte: ifte,
          track: track,
          initEnv: initEnv,
          $$eval: $$eval,
          evalCond: evalCond,
          checkEq: checkEq,
          makeEnv: makeEnv,
          updateEnv: updateEnv
        };
}

function ExtraOperators(Term) {
  var $tilde$neg = function (x) {
    return Term.$star(Term.c(-1.0), x);
  };
  var $neg = function (x, y) {
    return Term.$plus(x, $tilde$neg(y));
  };
  var $slash = function (x, y) {
    return Term.$star(x, Term.pow(y, -1.0));
  };
  var $plus$dot = function (xs, ys) {
    return Utilities.map2(xs, ys, Term.$plus);
  };
  var $neg$dot = function (xs, ys) {
    return Utilities.map2(xs, ys, $neg);
  };
  var $star$dot = function (xs, ys) {
    return Utilities.map2(xs, ys, Term.$star);
  };
  var $slash$dot = function (xs, ys) {
    return Utilities.map2(xs, ys, $slash);
  };
  var $great = function (x, y) {
    return Term.$less(y, x);
  };
  var $less$eq = function (x, y) {
    return Term.$pipe$pipe(Term.$less(x, y), Term.$eq(x, y));
  };
  var $great$eq = function (x, y) {
    return Term.$pipe$pipe(Term.$less(y, x), Term.$eq(x, y));
  };
  var $bang$eq = function (x, y) {
    return Term.not(Term.$eq(x, y));
  };
  var sigmoid = function (x) {
    return $slash(Term.c(1.0), Term.$plus(Term.c(1.0), Term.exp($tilde$neg(x))));
  };
  var sum = function (xs) {
    return Core__Array.reduce(xs, Term.c(0.0), (function (a, b) {
                  return Term.$plus(a, b);
                }));
  };
  var mean = function (xs) {
    return $slash(sum(xs), Term.c(xs.length));
  };
  var max = function (x, y) {
    return Term.ifte($great$eq(x, y), x, y);
  };
  var min = function (x, y) {
    return Term.ifte($less$eq(x, y), x, y);
  };
  var reLU = function (x) {
    return max(x, Term.c(0.0));
  };
  var leakyReLU = function (x) {
    var y = Term.c(0.0);
    return Term.ifte(Term.$less(y, x), x, Term.$star(Term.c(0.1), x));
  };
  var reELU = function (x) {
    return Term.ifte(Term.$less(x, Term.c(0.0)), Term.exp(x), x);
  };
  var dotproduct = function (v1, v2) {
    return Core__Array.reduce(Utilities.map2(v1, v2, Term.$star), Term.c(0.0), Term.$plus);
  };
  return {
          $tilde$neg: $tilde$neg,
          $neg: $neg,
          $slash: $slash,
          $plus$dot: $plus$dot,
          $neg$dot: $neg$dot,
          $star$dot: $star$dot,
          $slash$dot: $slash$dot,
          $great: $great,
          $less$eq: $less$eq,
          $great$eq: $great$eq,
          $bang$eq: $bang$eq,
          sigmoid: sigmoid,
          sum: sum,
          mean: mean,
          max: max,
          min: min,
          reLU: reLU,
          leakyReLU: leakyReLU,
          reELU: reELU,
          dotproduct: dotproduct
        };
}

export {
  MakeTerm ,
  ExtraOperators ,
}
/* Utilities Not a pure module */
