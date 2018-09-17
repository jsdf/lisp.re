# lisp.re

A Lisp in Reason. Based on [(How to Write a (Lisp) Interpreter (in Python))](http://norvig.com/lispy.html)

```scheme
lisp.re> (begin (define r 10) (* pi (* r r)))
=> 314.159265359

lisp.re> (define square (lambda (x) (* x x)))
=>
lisp.re> (square 2)
=> 4
```

The main source can be found in [this file](https://github.com/jsdf/lisp.re/blob/master/bin/lisp.re)

[Try it in the browser](https://jsdf.github.io/lisp.re/)

This codebase can be built for both Javascript and native targets.

# web

To build this repo for the Javascript/web target, first check it out, then:

```sh
yarn
yarn dev
```

Then open http://localhost:1234/

# native

Clone the repo and run these commands from within the project:

```sh
opam update # get the latest opam packages data. Skip this optionally
# opam will read into the `opam` file and add the other dependencies
opam install reason
opam install merlin
opam install re
make build    # build/rebuild your files
```

**Run**:

```sh
./_build/install/default/bin/lisp
```
