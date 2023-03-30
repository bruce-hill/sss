# Blang

Blang is a small, statically typed, garbage-collected, compiled imperative
language with ergonomic syntax. It uses
[libgccjit](https://gcc.gnu.org/wiki/JIT) as a backend, which compiles to a
binary.

## Example

```
def sing_bottles_song(n:Int)
    for i in n to 0 by -1
        if i == 0
            say "No more bottles of beer on the wall! :("
        else if i == 1
            say "One last bottle of beer on the wall"
        else
            say "$i bottles of beer on the wall"
    between
        say "Take one down, pass it around... "


sing_bottles_song 99
```

See [test/\*.bl](test/) for more examples.

## Usage

To build the compiler, simply run `make`. To install the compiler run `sudo
make install`.

The REPL can be run by running the command `blang` (or `./blang` in this
directory). Blang files can be run directly via `blang myprogram.bl`, compiled
to a static executable via `blangc myprogram.bl -o myprogram`, or compiled to a
library module via `blangc -c myprogram.bl -o libmyprogram.so`.

Additional command

## Language Features

[See features.md for writeups of some of the features in blang.](features.md) These include:

- Type-safe DSL strings
- Units of measure
- Better loops
- Ergonomic optional typing
- Memory safety (GC and automatic array bounds checking)
- Percentages

## Dependencies

The Blang compiler is written in C and uses
[libgccjit](https://gcc.gnu.org/onlinedocs/jit/) as the backend. Blang uses the
following libraries:

- Garbage collection: [Boehm garbage collector](https://www.hboehm.info/gc/)
    - Available from your package manager of choice: `pacman -S gc`
- String interning: [intern](https://github.com/bruce-hill/intern/)
    - Install via `git clone https://github.com/bruce-hill/intern/ && cd intern && make && sudo make install`

## Usage

Once the necessary dependencies are installed, you can use `./blang
your-file.bl` to run a file directly or `./blangc your-file.bl` to compile it
into a binary called `your-file`. See `blang --help` and `blangc --help` for
full usage info.

## License

Blang is released under the MIT license with the Commons Clause, see
[LICENSE](LICENSE) for full details.
