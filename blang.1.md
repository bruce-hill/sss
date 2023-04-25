% BLANG(1)
% Bruce Hill (*bruce@bruce-hill.com*)
% March 30, 2023

# NAME

blang - Bruce\'s Language

# SYNOPSIS

`blang` \[*options...*\] *program.bl* \[\[`--`\] *args...*\]

# DESCRIPTION

Blang is a programming language that is statically typed, compiled, small, and
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

`-l...`, `-W...`
: Linker flags beginning with `-l` or `-W` will be passed through to the linker.

`-I...`
: Add an option for the driver.

*args...*
: Extra arguments are passed to the compiled blang program when it runs.

