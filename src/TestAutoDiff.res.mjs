// Generated by ReScript, PLEASE EDIT WITH CARE

import * as AutoDiff from "./AutoDiff.res.mjs";

var MyTerm = AutoDiff.MakeTerm({});

var Op = AutoDiff.ExtraOperators(MyTerm);

var x = MyTerm.claim();

var y = MyTerm.claim();

var result = MyTerm.$$eval(x, [
      3.0,
      5.0
    ]);

MyTerm.checkEq("refx", result, {
      output: 3.0,
      derivative: [
        1.0,
        0.0
      ]
    });

var result$1 = MyTerm.$$eval(y, [
      3.0,
      5.0
    ]);

MyTerm.checkEq("refy", result$1, {
      output: 5.0,
      derivative: [
        0.0,
        1.0
      ]
    });

var result$2 = MyTerm.$$eval(MyTerm.$plus(x, y), [
      3.0,
      5.0
    ]);

MyTerm.checkEq("add", result$2, {
      output: 8.0,
      derivative: [
        1.0,
        1.0
      ]
    });

var result$3 = MyTerm.$$eval(Op.$neg(x, y), [
      3.0,
      5.0
    ]);

MyTerm.checkEq("sub", result$3, {
      output: -2.0,
      derivative: [
        1.0,
        -1.0
      ]
    });

var result$4 = MyTerm.$$eval(Op.$tilde$neg(y), [
      3.0,
      5.0
    ]);

MyTerm.checkEq("neg", result$4, {
      output: -5.0,
      derivative: [
        0.0,
        -1.0
      ]
    });

var result$5 = MyTerm.$$eval(MyTerm.$star(x, y), [
      3.0,
      5.0
    ]);

MyTerm.checkEq("mul", result$5, {
      output: 15.0,
      derivative: [
        5.0,
        3.0
      ]
    });

var result$6 = MyTerm.$$eval(Op.$slash(x, y), [
      3.0,
      5.0
    ]);

MyTerm.checkEq("div", result$6, {
      output: 0.6000000000000001,
      derivative: [
        0.2,
        -3.0 / 25.0
      ]
    });

var result$7 = MyTerm.$$eval(MyTerm.pow(x, 5.0), [
      Math.E,
      5.0
    ]);

MyTerm.checkEq("pow", result$7, {
      output: Math.exp(5.0),
      derivative: [
        5.0 * Math.exp(4.0),
        0.0
      ]
    });

var z = MyTerm.exp(x);

var result$8 = MyTerm.$$eval(z, [
      3.0,
      5.0
    ]);

MyTerm.checkEq("exp", result$8, {
      output: Math.exp(3.0),
      derivative: [
        Math.exp(3.0),
        0.0
      ]
    });

var z$1 = MyTerm.log(x);

var result$9 = MyTerm.$$eval(z$1, [
      3.0,
      5.0
    ]);

MyTerm.checkEq("log", result$9, {
      output: Math.log(3.0),
      derivative: [
        1.0 / 3.0,
        0.0
      ]
    });

var z$2 = Op.dotproduct([
      MyTerm.c(1.0),
      x,
      y
    ], [
      MyTerm.c(1.0),
      MyTerm.c(2.0),
      MyTerm.c(4.0)
    ]);

var result$10 = MyTerm.$$eval(z$2, [
      3.0,
      5.0
    ]);

MyTerm.checkEq("dotproduct", result$10, {
      output: 27.0,
      derivative: [
        2.0,
        4.0
      ]
    });

MyTerm.checkEq("ReLU-1", MyTerm.$$eval(Op.reLU(x), [
          3.0,
          5.0
        ]), {
      output: 3.0,
      derivative: [
        1.0,
        0.0
      ]
    });

MyTerm.checkEq("ReLU-2", MyTerm.$$eval(Op.reLU(Op.$neg(x, y)), [
          3.0,
          5.0
        ]), {
      output: 0.0,
      derivative: [
        0.0,
        0.0
      ]
    });

MyTerm.checkEq("leakyReLU-1", MyTerm.$$eval(Op.leakyReLU(x), [
          3.0,
          5.0
        ]), {
      output: 3.0,
      derivative: [
        1.0,
        0.0
      ]
    });

MyTerm.checkEq("leakyReLU-2", MyTerm.$$eval(Op.leakyReLU(Op.$neg(x, y)), [
          3.0,
          5.0
        ]), {
      output: -0.2,
      derivative: [
        0.1,
        -0.1
      ]
    });

if (!MyTerm.evalCond(MyTerm.$eq(x, y), [
        4.0,
        4.0
      ])) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "TestAutoDiff.res",
          192,
          0
        ],
        Error: new Error()
      };
}

if (!MyTerm.evalCond(MyTerm.not(MyTerm.$eq(x, y)), [
        3.0,
        5.0
      ])) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "TestAutoDiff.res",
          193,
          0
        ],
        Error: new Error()
      };
}

if (!MyTerm.evalCond(MyTerm.$less(x, y), [
        3.0,
        5.0
      ])) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "TestAutoDiff.res",
          194,
          0
        ],
        Error: new Error()
      };
}

if (!MyTerm.evalCond(MyTerm.not(MyTerm.$less(x, y)), [
        5.0,
        5.0
      ])) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "TestAutoDiff.res",
          195,
          0
        ],
        Error: new Error()
      };
}

if (!MyTerm.evalCond(MyTerm.$pipe$pipe(MyTerm.$eq(x, y), MyTerm.$less(x, y)), [
        3.0,
        5.0
      ])) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "TestAutoDiff.res",
          196,
          0
        ],
        Error: new Error()
      };
}

if (!MyTerm.evalCond(MyTerm.not(MyTerm.$amp$amp(MyTerm.$eq(x, y), MyTerm.$less(x, y))), [
        3.0,
        5.0
      ])) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "TestAutoDiff.res",
          197,
          0
        ],
        Error: new Error()
      };
}

MyTerm.checkEq("ifte-t", MyTerm.$$eval(MyTerm.ifte(MyTerm.$less(MyTerm.c(1.0), MyTerm.c(2.0)), x, y), [
          3.0,
          5.0
        ]), {
      output: 3.0,
      derivative: [
        1.0,
        0.0
      ]
    });

MyTerm.checkEq("ifte-f", MyTerm.$$eval(MyTerm.ifte(MyTerm.$less(MyTerm.c(9.0), MyTerm.c(2.0)), x, y), [
          3.0,
          5.0
        ]), {
      output: 5.0,
      derivative: [
        0.0,
        1.0
      ]
    });

export {
  MyTerm ,
  Op ,
  x ,
  y ,
  z$2 as z,
  result$10 as result,
}
/* MyTerm Not a pure module */
