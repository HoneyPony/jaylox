# jaylox
jaylox is a somewhat-optimizing compiler for Bob Nystrom's Lox programming language. It compiles Lox code to C code, which can then be compiled by a C compiler into a runnable executable.

The C compiler itself is able to optimize the code to be quite fast, but it gets some help from the jaylox compiler -- the jaylox compiler performs several small optimizations to make the code faster. These include optimizations to the data structures used to represent classes so that field lookup is fast, as well as optimizations where values of a known type are stored as C values of that type rather than as full Lox-compatible dynamic values.

## Building
jaylox has libtcc support for running scripts in-memory. However, this can require some custom RUSTFLAGS changes. So, compilation is performed with a Makefile. If you just want to build the compiler without libtcc support, you should run:

```sh
make
```

Otherwise you can edit the file .config as such:
```Makefile
RUSTFLAGS="-L xyz" # If you need to add link paths
FEATURES=--features run
```

to enable the "-run" flag.

## Running

The command line flags are unfortunately not very good right now. To compile a Lox program in the best default configuration, run:

```sh
./jaylox -backtrace -enablenames -nanbox <myscript.lox>
```

(note that all command line arguments must come before the Lox file, for now).

This will create an executable called "myscript." Other command line options you can use:

- `-run` -- compile the script using libtcc and immediately execute it. Requires `--features run`.
- `-o <output>` -- compile the script into the given executable file. This requires a `gcc` on your PATH.
- `-oc <output.c>` -- compile the script into the given C file. Note by default this C file will have all of jaylib/jaylib.h pasted into it.
- `-os` -- compile the script into C, and output it to standard output. Also note that by default the C file will have all of jaylib/jaylib.h pasted into it.
- `-O1` or `-O2` or `-O3` -- passes the same flag to the C compiler (if generating an executable output). So, these do gcc optimization levels.
- `-nanbox` -- enable NaN boxing. This usually speeds up scripts.
- `-externjaylib` -- instead of pasting jaylib.h into the C file, just output `#include "jaylib/jaylib.h"` where it would be pasted.
- `-backtrace` -- enable backtraces when there is an error. This slows down the code slightly, as the C code will update a variable each time the line of code changes.
- `-enablenames` -- enable names for classes, functions, and fields. This slows down the code slightly, as this information is otherwise not tracked.
- `-conformance` -- turns on "full conformance mode," which disables some optimizations (so that errors occur at runtime instead of compile time) and slight semantic changes. Usually not necessary unless running the tester.
- `-assumecorrect` -- a fun option that deletes all type checking from the C code, i.e. it assumes your Lox script has no runtime errors. This can slightly speed things up, but does mean any runtime error will likely have unpredictable consequences.
- `-gcstress` -- stress tests the GC by making it collect every time an allocation occurs (and reduces the initial heap size to be tiny).

## Running the tests
Some shell scripts in the repo are set up to run the test suite from the craftinginterpreters repo. To do this, run:

```
./fetch-tester.sh
./run-tests.sh
```

Note that this requires a Dart installation (with an older version) -- I've tested it with Dart 2.12.0.

`fetch-tester.sh` does a `git clone` of the craftinginterpreters repo into the `extern` directory; `run-tests.sh` runs the tester using dart.

Any of the tests in the "tests" directory will be copied into the `extern/craftingintepreters/test/jaylox` directory whenever you run `fetch-tester.sh`, so it's possible to add new tests specifically for jaylox.

## Project status
jaylox currently passes all of the tests from the craftinginterpreters repo. It has sufficient builtin functions to run James Hamilton's [Lox.lox](https://github.com/mrjameshamilton/loxlox/tree/main), but there appears to be a compiler error preventing the code from functioning. So, the compiler still has some bugs.
