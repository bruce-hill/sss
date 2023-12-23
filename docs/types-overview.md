# Type System Overview

This is a quick overview of SSS's type system. SSS's type system is designed
with a few goals in mind:

- Simplicity: you shouldn't need to understand category theory to understand
  SSS's type system, things should generally be obvious and easy to
  understand.
- Helpful: SSS's type system is intended to help you catch simple user errors
  and potential bugs, like passing the wrong type to a function.
- Performance-oriented: SSS's type system is meant to reflect the underlying
  hardware implementation of its types. As such, there is a clear distinction
  between pointers and values. Structs and tagged unions in SSS are directly
  equivalent to structs and tagged unions in C, so interoperability with C is
  easy.
- Memory-safe: SSS's type system distinguishes between potentially NULL
  pointers and pointers that are guaranteed to be non-NULL. Dereferencing
  potentially NULL pointers is a compile-time error.
- Expressive: SSS hopes to make it easy to differentiate between different
  types with the same underlying representation. For example, a string that
  represents a JSON object vs. a string that represents an SQL query, or a
  floating point number that represents a velocity vs. a floating point number
  that represents a distance. These distinctions should be checked only at
  compile time and introduce no runtime overhead.

## Type Inference

All expressions in SSS have a trivially-inferred type that follows some simple rules:

- Literal values have the type that you would expect them to have. A string
  literal has type `Str`, a boolean literal has type `Bool`, etc.
- Integer literals default to `Int` and floating point literals default to `Num`.
- Explicit casts can be used to specify lower precision values (e.g. `5:Int32`).
- Local variables are always declared with an initial value. The variable's
  type is whatever the initial value's type inferred to be.
- Functions are declared with an explicit return type.
- Function arguments are declared with an explicit type.
- In the special case of nil values and empty arrays, types must be specified
  explicitly.

This means that SSS requires explicit type annotations for function and
datatype definitions, but uses type inference for everything else. Function
type annotations serve as helpful documentation and also checkpoints to ensure
the compiler is doing what you think it should be doing. However, variable
declarations shouldn't need type annotations if it's obvious what.

## Primitives

Primitive types are numeric types that the computer understands:

- Integer types: `Int` (64-bit signed integer), `Int32` (32-bit signed), `Int16` (16-bit signed), `Int8` (8-bit signed)
- Characters: `Char`
- Booleans: `Bool`
- Floating point numbers: `Num` (64-bit float), `Num32` (32-bit float)

### Automatic Promotion

For convenience, numeric promotion is allowed when promoting from
lower-precision numeric types to higher-precision. For example, adding an
`Int16` and an `Int32` results in an `Int32`. Or, you can initialize a struct
with `Num` fields using integer literals. The type of a numeric literal without
any decimal point is always `Int`, so automatically converting that to a
floating point value (instead of giving a type error) is essential for
ergonomics.

### Using Integer Literals Without a Suffix

In places where the compiler knows to expect a certain type of integer value
(e.g. an `Int8`), you can use integer literals without a suffix and the
compiler will make everything work out okay. For example, if you declare `x :=
5i8`, you can do a comparison like `(x > 0)` or an assignment like `x += 1` and
the compiler will make everything work (the `0` and `1` in these examples will
be compiled to an 8-bit integer). The only exception is if the integer literal
is too large to fit in the required type, in which case, you will get a
compiler error.

## Structs

Structs in SSS are similar to C: a packed blob of data with named fields of different types:

```sss
type Vec2 := struct(x,y:Num)
my_vec := Vec2{1.5, 2.3}
equivalent := Vec2{x=1.5, y=2.3}
>>> my_vec == equivalent
=== yes
```

Structs are referenced by name when using them in type annotations:

```sss
func foo(v:Vec2)->Num
    return v.x + v.y
```

Structs can be recursive or co-recursive if needed, although a struct can only
contain a _pointer_ to structs with the same type as itself, otherwise the
struct would need to be infinitely large. All struct fields must be explicitly
populated during instantiation, unless the fields are optional pointers or have
a default value.

## Pointers

Pointers in SSS represent a reference to a blob of data that lives somewhere in
memory. SSS is garbage collected, requires explicit NULL checks for potentially
NULL pointers, and performs runtime bounds checks on array accesses, so SSS's
pointers are considerably safer than C's pointers. However, the basic concept
is similar. In order to instantiate a heap-allocated object, use the `@`
(mnemonic: **a**llocation) operator. `@foo` in SSS is similar to
`memcpy(GC_malloc(sizeof(foo)), &foo)` in C.

```sss
heap_vec := @Vec2{1.2, 3.4}
```

Every instantiation of a pointer is guaranteed to point to valid program memory
and the memory pointed to by a pointer will never be invalid. The only
exception to this rule is if a pointer is created by casting an integer value
to a pointer, which should be done rarely and only with extreme caution.

## Optional Pointers

Sometimes, you want to have a value that represents either a value or the
absence of a value. In such cases, optional pointers are used. Rather than a
cumbersome `Optional(Foo)` type (with associated pattern matching), SSS uses
a `?` unary prefix operator to indicate a heap-allocated optional value. `!T`
is used as a literal value to represent a `T` pointer that doesn't point to
anything.

```sss
v := ?Vec2{1, 2} // The variable v may or may not point to a value
if random() mod 2 == 0
    v = !Vec2

func say_x(v:@Vec2)
    say "$(v.x)"

say_x(v) // Compiler error: `v` is type `?Vec2`, not `@Vec2`
```

There are three main ways to handle optional values:

- The first, simplest way, is to convert an optional value into a guaranteed
  non-nil value by providing a fallback value: `v or @Vec2{0,0}`. The compiler
  will treat this expression as having type `@Vec2`, allowing it to be safely
  used as a non-nil pointer. `or` operations are evaluated lazily, so the
  default value code will only be executed in the event that `v` is nil.

- The second way is to halt the program if the value is nil or assume that it's
  non-nil if the program hasn't halted. For example, the expression `v or fail
  "'v' was unexpectedly nil"` has the non-nil pointer type `@Vec2`. This can be
  useful when working with APIs that only return nil values when something has
  gone seriously and irrecoverably wrong.

- The final way is to do a conditional branch that handles a non-nil value in
  one branch and handles a nil value in the other branch. SSS supports this
  by allowing declarations in conditional expressions like so:

```sss
// Conditional branch:
if v2 := v
    // `v2` has type `@Vec2`, i.e. a guaranteed non-nil pointer
    say_x(v2) // Okay
else
    say "Oh, it was nil"
```

If desired, the same variable can be shadowed with a non-nil version of itself
by doing `if foo := foo`.

These techniques allow for safely and explicitly handling optional values
without the boilerplate overhead of requiring pattern matching in every case
where non-optional values need to be produced from an optional value. 

## Stack Pointers

SSS also permits taking a value that lives on the stack and using its address
as a pointer. Stack pointers use the sigil `&` and are subject to extra
restrictions that ensure memory safety:

- Stack pointers cannot be stored in heap memory
- Stack pointers can be passed as arguments to functions, but cannot be
  returned from functions
- Heap pointers (`@`) can be used in places where a stack pointer (`&`) is
  needed, but not the other way around.

These restrictions are necessary because stack memory is "freed" as soon as the
current function call returns, so it is not valid to hold on to a pointer to
that memory beyond when the function returns. In general, if a function does
not need to store a pointer in heap memory or return it, it can be helpful to
use a function signature with `&`, which permits callers to use either `&`
pointers or `@` pointers.

The `&` operator, when used on a local variable with type `Foo`, will return a
pointer with type `&Foo`. When used on a field or index in heap allocated
memory, will return a pointer with type `@Foo`.

```sss
x := 123
>>> &x
=== &123 : &Int

foo := @Foo{x=99}
>>> &foo.x
=== @99 : @Int
```

# Arrays

Arrays in SSS represent a fixed-size, bounds-checked, 1-indexed, ordered
collection of values. The underlying implementation uses a struct value similar
to `Array{data:@T, size:Int32, stride:Int16, ...}`, where `array.data` is a
pointer to a heap-allocated chunk of memory containing items of whatever type
is inside the array. The type of the array is inferred from the type of its
members, or, in the case of an empty array, must be explicitly specified.

```sss
ints := [1,2,3,4]
strings := ["hi", "bye"]
empty := [:Str]
```

Arrays define a few basic operations: iteration, indexing, and length querying:

```sss
for str in strings
    say "$str"

>>> strings.length
=== 2
>>> strings[2]
=== "bye"
```

Array indexing is **1-indexed** and bounds-checked at runtime. Any attempts to
access a value outside of the array (`index == 0` or `index > #array`) will
result in a runtime failure with an informative error message detailing what
index the code was attempting to access and how big the array is. Negative
array indices are treated as back-indexed, so `-1` is the last element of the
array, `-2` is the second-to-last, etc. A negative index with magnitude greater
than the length of the array will also produce a runtime error.

Arrays can also be sliced, resulting in a new array struct that references the
original array's memory (but with a possibly internal data pointer and possibly
different length and stride values).

# Strings

Strings in SSS are represented as arrays of characters.

# Functions

Functions in SSS use explicit type annotations.

