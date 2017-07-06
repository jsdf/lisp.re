# lisp.re

A Lisp in Reason.

```scheme
lisp.re> (begin (define r 10) (* pi (* r r)))
=> 314.159265359
```

to build and run this repo, follow the [Reason native workflow](http://facebook.github.io/reason/nativeWorkflow.html), then:

```bash
opam update
opam pin add -y lisp_re .
make run
```

# Tutorial (how to create this from scratch)

## Install

Clone the repo and install the dependencies:

```sh
git clone https://github.com/reasonml/ReasonNativeProject.git
cd ReasonNativeProject
opam update # get the latest opam packages data. Skip this optionally
# opam will read into the `opam` file and add the other dependencies
opam pin add -y ReasonNativeProject .
```

Now we'll customize the project.

### Customizing ReasonNativeProject

The source will live in a single Reason file: `./bin/lisp.re`. Rename the existing `./bin/test.re` to `./bin/lisp.re`. Change the contents of the file to:

```reason
print_endline "Hello Reason Native";
```

There is also a jbuild file alongside the Reason source file. This contains instructions to the build system. Change the contents of `./bin/jbuild` to:

```lisp
(jbuild_version 1)

(executable
 ((name lisp)
  ; This will be installable as a global binary
  (public_name lisp)))
```

Run `make build` to build the project.

The built output is in `_build`. Try running it with `_build/default/bin/lisp.exe`.

You can also just run `make run`.

You should see the output `Hello Reason Native`.

##### Add Another Dependency

You'll want to make use of existing libraries in your app, so browse the growing set of `opam` packages in the [opam repository](http://opam.ocaml.org/packages/).

To add a dependency you need to edit your `ReasonNativeProject.opam` file so that you depend on a particular opam package and range of versions. For example:

```
depends: [
  "jbuilder" {build}
  "reason"  {= "2.0.0"}
  "containers"  {>= "1.2"}
]
```
Here we've added the 'containers' library, which is basically an extension of the (very austere) OCaml standard library. We've specified that at least version 1.2 must be used.

Now run

```bash
opam pin add -y ReasonNativeProject .
```

to install the new dependency.


Change the contents of `./bin/jbuild` to:

```lisp
(jbuild_version 1)

(executable
 ((name lisp)
  ; This will be installable as a global binary
  (public_name lisp)
  ; and it depends on a library from opam
  (libraries (containers))))
```

In `./bin/lisp.re`:

```reason
open Containers;

print_endline "Hello Reason Native";
```

`make run` and you should see the same output again. If you get an error about `Containers`, you might need to try installing the dependencies again.

## Troubleshooting

In general, if something goes wrong, try upgrading your install of the project
by running `opam upgrade ReasonNativeProject`, or if it failed to install and you
later fixed it, `opam install ReasonNativeProject`.
