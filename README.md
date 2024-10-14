# ReLearn: deep learning in ReScript

This project aims to provide

* a typed deep learning framework
* that is easy to understand
* that is fast enough to do MNIST 80% correct within 15 minute in a modern browser.

## Why a new framework?

I am taking CSCI 2470, a Deep Learning course at BrownU. One of the best part of the course is HW3, which asks students to implement a mini Keras. The course is nice but the implementation and the math are hard to understand: lots of the implementation are dealing with GradientTape, weights vs inputs, getter and setter, etc. None of those details are relevant to interesting concepts such as auto-diff and gradient-descend. For example, the implementation of `softmax` takes a dozen lines in the implementation, while its mathematical form barely fill one line.

With research experience in the area of Programming Languages, I am unhappy with the verbose implementation that apparently doesn't match closely with the essence. This project, **ReLearn**, aims to provide a concise (and hence to-the-point) deep learning framework. In particular, `softmax` takes only five well-indented lines, where the extra information are solely about being explicit in types and reducing computational cost:

```rescript
  let softmax = xs => {
    let exs = Array.map(xs, x => exp(x))
    let sexs = sum(exs)
    Array.map(exs, ex => ex / sexs)
  }
```

## Concepts

A deep learning task is that given a dataset, find the function that best describe the data under certain constrains. The constraints limit the form and placeholder within the functions.

A **dataset** is an array of **datum**. Each datum has an **input** and an **output**. Multiple inputs? Wrap them in a tuple or an array!

A **DataSchema** (`module type DataSchema`) describes the datasets that share the same input type and output type.

A **Model** (`module type Model`) is, very roughly speaking, a function from input to output. But there are many extra information to keep along the side:

* `module Dataset`: The **DataSchema** of the supported dataset
* `module Term`: You can consider it as an auto-diff-able subset of ReScript. Because it is auto-diff-able, it necessarily contains parameters, and need an environment (`AutoDiff.env`) to figure out the meaning of the parameters. This is why the `mode` function (explained below) takes an `AutoDiff.env` in addition to the distribution.
* `type distribution`: The model's representation of the **output distribution**.
* `let f`: A function `f` from an input to a distribution of the output.
* `let loss`: The loss function.
* `let mode`: The function computes the mode from the distribution. This function is useful when we use the model to make prediction.

## What is here?

The folder `datasets` contains datasets of interest.
Currently, it contains only the [CIFAR-10](http://www.cs.toronto.edu/~kriz/cifar.html) dataset.
