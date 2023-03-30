% BLANG(1)
% Bruce Hill (*bruce@bruce-hill.com*)
% March 30, 2023

# NAME

blang - Bruce\'s Language

# SYNOPSIS

`blang` \[*options...*\] *program.bl* \[\[`--`\] *args...*\]

`blangc` \[*options...*\] *program.bl*

# DESCRIPTION

Blang is a small, statically typed, garbage-collected, compiled imperative
language with ergonomic syntax. It uses libgccjit as a backend, which compiles
to a binary.

# OPTIONS

`-h`, `--help`
: Print the usage and exit.

`-v`,`--verbose`
: Add verbose output.

`-a`,`--asm`
: Output assembly to the console during compilation.

`-c`
: Compile the program as a module, rather than a program.

`-O` *level*
: Set the optimization level.

*args...*
: Extra arguments are passed to the compiled blang program when it runs.

