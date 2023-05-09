% SSS(1)
% Bruce Hill (*bruce@bruce-hill.com*)
% March 30, 2023

# NAME

sss - A language that prioritizes Safety, Simplicity, and Speed.

# SYNOPSIS

`sss` \[*options...*\] *program.sss* \[\[`--`\] *args...*\]

# DESCRIPTION

SSS is a programming language that is statically typed, compiled, small, and
garbage-collected, with concise syntax and built-in support for
high-performance, low-overhead datastructures. It uses libgccjit as a backend,
which can produce compiled binaries or run files directly.

# OPTIONS

`-h`, `--help`
: Print the usage and exit.

`-v`,`--verbose`
: Add verbose output.

`--version`
: Print the version and exit.

`-A`,`--asm`
: Output assembly to the console during compilation.

`-c`
: Compile the program to a file, rather than running it.

`-O` *level*
: Set the optimization level.

`-o` *file*
: Specify the output file when compiling to a file.

`-e`,`--eval` *expr*
: Evaluate an expression passed in as a command line argument and print the result.

*args...*
: Extra arguments are passed to the compiled SSS program when it runs.

