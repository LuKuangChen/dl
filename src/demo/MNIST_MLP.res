open Utilities
open Concepts

module MyModel: Model = {
  module DataSchema = {
    type input = array<array<float>> // that is 28*28
    type output = int // that is 0-9
    type datum = {input: input, output: output}
  }
  open! DataSchema

  module Term = AutoDiff.MakeTerm()
  module ExtraOps = AutoDiff.ExtraOperators(Term)

  type distribution = array<Term.term>

  let init = () => Math.random() -. 0.5
  let n1 = 28 * 28
  let n2 = 28
  let n3 = 10

  open! Term
  open! ExtraOps
  let wbs1 = buildArray(n2, _ => {
    (claimMany(n1, ~init), claim(~init))
  })
  let wbs2 = buildArray(n3, _ => {
    (claimMany(n2, ~init), claim(~init))
  })
  let f: input => array<term> = input => {
    let input = Array.flat(input)
    let input = Array.map(input, c)
    let input = wbs1->Array.map(((w, b)) => {
      dotproduct(input, w) + b
    })
    let input = wbs2->Array.map(((w, b)) => {
      dotproduct(input, w) + b
    })
    softmax(input)
  }
  let mode: (distribution, AutoDiff.env) => output = (dist, env) => {
    eval(argmax(dist), env).output->Int.fromFloat
  }
  let loss = (output: array<term>, answer) => output[answer]->Option.getExn->log
}
