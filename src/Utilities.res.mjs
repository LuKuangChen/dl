// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Core__Array from "@rescript/core/src/Core__Array.res.mjs";
import * as Core__Option from "@rescript/core/src/Core__Option.res.mjs";
import * as RescriptCore from "@rescript/core/src/RescriptCore.res.mjs";

function buildArray(length, f) {
  return Core__Array.make(length, 0.0).map(function (param, i) {
              return f(i);
            });
}

function map2(ns, ms, f) {
  if (ns.length !== ms.length) {
    throw {
          RE_EXN_ID: "Assert_failure",
          _1: [
            "Utilities.res",
            7,
            2
          ],
          Error: new Error()
        };
  }
  return ns.map(function (n, i) {
              var m = Core__Option.getExn(ms[i], undefined);
              return f(n, m);
            });
}

function jitter(n) {
  if (Math.random() >= 0.5) {
    return n - Number.EPSILON;
  } else {
    return n + Number.EPSILON;
  }
}

function closeEnough(n, m) {
  return Math.pow(n - m, 2.0) < Number.EPSILON;
}

function vadd(x, y) {
  return map2(x, y, (function (x, y) {
                return x + y;
              }));
}

function vsub(x, y) {
  return map2(x, y, (function (x, y) {
                return x - y;
              }));
}

function vmul(x, y) {
  return map2(x, y, (function (x, y) {
                return x * y;
              }));
}

function vdiv(x, y) {
  return map2(x, y, (function (x, y) {
                return x / y;
              }));
}

function eadd(x, y) {
  return x.map(function (x) {
              return x + y;
            });
}

function esub(x, y) {
  return x.map(function (x) {
              return x - y;
            });
}

function emul(x, y) {
  return x.map(function (x) {
              return x * y;
            });
}

function ediv(x, y) {
  return x.map(function (x) {
              return x / y;
            });
}

function dotproduct(v1, v2) {
  return Core__Array.reduce(map2(v1, v2, (function (prim0, prim1) {
                    return prim0 * prim1;
                  })), 0.0, (function (prim0, prim1) {
                return prim0 + prim1;
              }));
}

function isGoodNumber(x) {
  return !(!isFinite(x) || isNaN(x));
}

function checkGoodNumber(x, message) {
  if (!isGoodNumber(x)) {
    return RescriptCore.panic(message + " is not a good number: " + x.toString());
  }
  
}

function checkedOp1(name, f) {
  return function (x) {
    checkGoodNumber(x, name + "'s argument");
    var o = f(x);
    checkGoodNumber(o, name + "'s output (input is " + x.toString() + ")");
    return o;
  };
}

function checkedOp2(name, f) {
  return function (x, y) {
    checkGoodNumber(x, name + "'s first argument");
    checkGoodNumber(y, name + "'s second argument");
    var o = f(x, y);
    checkGoodNumber(o, name + "'s output (inputs are " + x.toString() + " and " + y.toString() + ")");
    return o;
  };
}

var max = checkedOp2("max", (function (x, y) {
        if (x >= y) {
          return x;
        } else {
          return y;
        }
      }));

var min = checkedOp2("min", (function (x, y) {
        if (x <= y) {
          return x;
        } else {
          return y;
        }
      }));

var pow = checkedOp2("pow", (function (x, exp) {
        return Math.pow(x, exp);
      }));

var exp = checkedOp1("exp", (function (prim) {
        return Math.exp(prim);
      }));

var log = checkedOp1("log", (function (prim) {
        return Math.log(prim);
      }));

var CheckedMath = {
  max: max,
  min: min,
  pow: pow,
  exp: exp,
  log: log
};

export {
  buildArray ,
  map2 ,
  jitter ,
  closeEnough ,
  vadd ,
  vsub ,
  vmul ,
  vdiv ,
  eadd ,
  esub ,
  emul ,
  ediv ,
  dotproduct ,
  isGoodNumber ,
  checkGoodNumber ,
  checkedOp1 ,
  checkedOp2 ,
  CheckedMath ,
}
/* max Not a pure module */
