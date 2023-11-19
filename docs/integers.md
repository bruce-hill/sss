# Integer Types in SSS

In SSS, the following integer or integer-like types exist:

- Bool: "1-bit"
  - The only values are `yes` (true) and `no` (false)
  - Bool values only support logical operations (`and`, `or`, `xor`, `not`)
- Byte: 8 bits, unsigned, values 0-255
  - Currently not implemented
  - Byte values support bitwise and math operations
    - Math operations have wraparound (i.e. `Byte.max + Byte::1 == Byte::0`)
  - Prints as `123_b`
- Char: 8 bits, unsigned, values 0-255
  - Char values do not support any math, bitwise, or logical operations
  - Prints as ``` `A ```
- Codepoint: 32 bits, unsigned, representing a unicode codepoint
  - Currently not implemented
  - Codepoint values do not support any math, bitwise, or logical operations
  - Prints as `U+000065`
- Int8, Int16, Int32, Int (implied 64-bit): N-bit signed integers
  - Int* types support bitwise and logical operations
    - Overflow/underflow of Int* operations triggers a runtime error that halts the program
      - E.g. `Int.max + 1`
    - Int* values are automatically promoted to higher-precision integer types
      - Some examples:
        - A math operation between two different integer types yields a result with
          the larger type (e.g. Int8 + Int32 -> Int32)
        - If a function takes a 64-bit integer and you pass it a 32-bit integer,
          the argument will be automatically promoted
  - Prints as `123_i8`, `123_i16`, `123_i32`, `123` (implied 64-bit)

All of these types support comparisons and can be used as keys in a table.

By default, any integer types whose precision is not specified will have type
`Int` (64-bit).

## Operations

Since no types support both both logical *and* bitwise operations, SSS uses the
operators `and`, `or`, `xor`, and `not` for both.

Operation       | SSS Types      |SSS Code    | C Code
----------------+----------------+------------+---------------
Logical AND     | `Bool`         | `x and y`  | `x && y`
Bitwise AND     | `Byte`, `Int*` | `x and y`  | `x & y`
Logical OR      | `Bool`         | `x or y`   | `x || y`
Bitwise OR      | `Byte`, `Int*` | `x or y`   | `x | y`
Logical XOR     | `Bool`         | `x xor y`  | N/A
Bitwise XOR     | `Byte`, `Int*` | `x xor y`  | `x ^ y`
Logical NEGATE  | `Bool`         | `not x`    | `!x`
Bitwise NEGATE  | `Byte`, `Int*` | `not x`    | `~x`
Addition        | `Byte`, `Int*` | `x + y`    | `x + y`
Subtraction     | `Byte`, `Int*` | `x - y`    | `x - y`
Multiplication  | `Byte`, `Int*` | `x * y`    | `x * y`
Division        | `Byte`, `Int*` | `x / y`    | `x / y`

## Relation to Floating Point

All `Int*` types can be automatically promoted to any precision of floating
point number. However, floating point numbers can only be manually converted to
integer types using the `as` operator.
