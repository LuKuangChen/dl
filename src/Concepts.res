module type Dataset = {
    type input
    type output
    type datum = {
        input, output
    }
    type t = array<datum>
}

module type Term = {
    type t<'domain>
    type env
    let eval: (t<'domain>, env) => 'domain
    let run: (t<float>, env) => (float, env)
}

module type Model = (Dataset: Dataset, Term: Term) => {
    let nParameters: int
    type t = (Dataset.input) => Term.t<Dataset.output>
}

module type Loss = (Dataset: Dataset) => {
    type t = (~predicted:Dataset.output, ~expected:Dataset.output) => float
}

module type Optimizer = (Term: Term) => {
}