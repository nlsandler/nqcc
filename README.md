# NQCC - a not-quite-C compiler

**This repo is no longer active. I'm working on a new version of NQCC - I'll link to it here once it's released!**

A compiler for a tiny (but growing!) subset of C, written in OCaml.

## Requirements
* Install OCaml and OPAM
* Configure OPAM
````
opam init
````

* Install dependencies:
```
opam install oasis
opam install batteries
opam install ounit # only needed for tests
````

## Usage
* Build it
```
make
````

* To run it, invoke the `nqcc` script in the project root
```
./nqcc /path/to/source.c
```
The compiled executable will be in the same directory as the source file, and have the same name (e.g. `source` in the example above).

## Tests
```
ocaml setup.ml -configure --enable-tests # you only need to run this once
make test # run unit tests
./test_examples.sh # compile sample programs
````

You can also test against [this test suite](https://github.com/nlsandler/write_a_c_compiler).
