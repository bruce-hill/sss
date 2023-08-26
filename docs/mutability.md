# Mutability and Value Semantics

All SSS values are either immutable values with value semantics or addresses of
heap-allocated memory whose contents may change over time (reference
semantics). The sigil for the address of mutable, heap-allocated memory is the
`@` symbol (mnemonic cue: "@" for **a**ddress of memory).

## Primitive Values

The simplest form of immutable values are primitives like integers, floating
point numbers, and boolean values. What does immutability and value semantics
mean in this context? Pretty much what you'd expect: a variable can hold a
primitive value like `5`, but there is no way to mutate the `5` itself. Since
variables aren't constant, you can assign different immutable values to a
variable, but changing one variable doesn't affect other variables.

```
x := 5
five := x

>>> x == five
=== yes

// Assign a new value to x:
x = 999

>>> x
=== 999

// The number "5" hasn't changed, only the binding of "x"
>>> five
=== 5

```

So far, none of this should be remotely surprising or interesting. This is
how every language with variables handles primitive values.

## Structs

The next case is slightly more complicated: structs (i.e. structures). These
are equivalent to C's `struct`s. Functionally, all of the same intuitions about
primitive values should apply here. Two structs are equal when their members
are equal.

```
struct Vec{x,y:Num}

v := Vec{1, 2}
copy := v
>>> v == copy
=== yes

v = Vec{100, 200}
>>> copy
=== Vec{1, 2}

>>> v == copy
=== no

// Compile error: struct values can't be modified
v.x = 99
```

Struct values can't be mutated directly, but variables holding struct values
can be modified with various operations.

```
v := Vec{1, 2}
copy := v

v *= 100
>>> v
=== Vec{100, 200}

v += Vec{1, 1}
>>> v
=== Vec{101, 201}

// `copy` hasn't changed, only which value is in `v`
>>> copy
=== Vec{1, 2}
```

These operations are equivalent to assigning a new struct value to `v` and do
not violate the rule that struct values are immutable.


## Arrays

Arrays are one more slight complication to the value semantics of SSS. Array
values and array slices are considered immutable values just like primitive
values and structs. However, for implementation efficiency and simplicity
reasons, the member values of an array are stored on the heap. However, the
member list of an array value is guaranteed to be immutable once the array has
been constructed. This means that the length, stride, and contents of an array
value will never change. As a result, arrays follow the same value semantics as
primitive values and structs.

```
arr := [10, 20, 30]
copy := arr

>>> arr == copy
=== yes

>>> arr == [10, 20, 30]
=== yes

arr = [40, 50]
>>> arr == copy
=== no

// Compiler error: arr is an Array value that cannot be modified
arr[1] = 999
```

### Slicing

Arrays may be "sliced", which creates a new array value representing some
subset of an existing array, perhaps in a different order.

```
arr := [10, 20, 30, 40]
>>> arr[2 to.. by 2]
=== [20, 40]
```

Internally, arrays are represented as a struct that stores a data pointer, an
element count (length), and a stride (AKA "step") by which traversal occurs.
Array slices are created _without_ copying the data behind the data pointer,
but instead creating a new struct value with the appropriate data pointer,
element count, and stride. This means that slices can be created in constant
time, without the need to copy array contents, even in the case of backwards
slices or slices that skip over elements.

Array equality is determined by examining the array contents when the
corresponding stride, length, and data pointers don't match. This means that
two arrays are considered equal if their contents are equal, regardless of
whether they point to the same heap memory (structural equality).

```
array := [10, 20, 30, 40]
slice := array[2 to.. by 2]
>>> slice
=== [20, 40]

>>> slice == [20, 40]
=== yes
```

### Strings

Since strings are simply an array whose members have type `Char`, strings have
value semantics exactly the same as other array types.

```
str := "Hello"
copy = str
>>> str == copy
=== yes
>>> str == "Hello"
=== yes
>>> str = "olleH"[..to.. by -1]
=== yes
```


## Addresses

The last case to consider is memory addresses. Memory addresses refer to
specific locations in memory that may hold primitive values. The address itself
is a value referring to particular position in memory, and so it has value
semantics. However, the value of an address is entirely separate from what
contents reside at that memory address. You can think of it a little bit like a
file path. Two file paths are equal if and only if they refer to the same file.
Two file paths are _not_ equal just because they point to two different files
that happen to have the same contents.

```
// File 1: /home/users/Alice/my_favorite_movie.txt
The Good, the Bad, and the Ugly
```

```
// File 2: /home/users/Bob/my_least_favorite_movie.txt
The Good, the Bad, and the Ugly
```

Is the file `/home/users/Alice/my_favorite_movie.txt` equal to the file
`/home/users/Bob/my_least_favorite_movie.txt`? Of course not. Although the
contents of the two files currently match each other, writing to one file has a
different behavior from writing to the other file. However, it should be
obvious that the file `/home/users/Alice/my_favorite_movie.txt` is equal to
_itself_. Two references to the same file are interchangeable when it comes to
reading and writing behavior.

Memory addresses in SSS work the same way. Two memory addresses are equal if
and only if they refer to the same address, it doesn't matter what the contents
of that memory region are. This is known as referential equality (as opposed to
structural equality).

```
struct Vec{x,y:Num}

v1 := @Vec{1, 2}
>>> v1
=== @Vec{1, 2}

// This holds the same memory address:
same_ref := v1
>>> same_ref
=== @Vec{1, 2}

// Referential equality:
>>> v1 == same_ref
=== yes

v1.x = 999
>>> same_ref
=== @Vec{999, 2}

same_ref.x = 456
>>> v1
=== @Vec{456, 2}

// Heap allocated values do *not* have structural equality
a := @Vec{1, 2}
b := @Vec{1, 2}

// `a` and `b` refer to different memory addresses:
>>> a == b
=== no

a.x = 999
>>> a
=== @Vec{999, 2}
>>> b
=== @Vec{1, 2}
```

What do we do if we want to compare the contents of two memory addresses to see
if they hold the same value? We simply have to _dereference_ the memory to
examine the contents:

```
a := @Vec{1, 2}
b := @Vec{1, 2}

>>> a == b
=== no

// Dereference memory to compare contents:
>>> a[] == b[]
=== yes
```

Memory addresses allow for you to create mutable objects. Or, more precisely,
mutable objects can be implemented as references to heap-allocated memory cells
that can hold different values over time.


### Mutable Arrays

Arrays have exactly the same value semantics as structs when it comes to
heap-allocated array addresses. However, the underlying implementation is
slightly more complicated. When a heap-allocated memory address is created for
an array, an extra information field is added that is used to flag an array as
"copy on write." Whenever a slice is taken of an array that lives in
heap-allocated memory, the array is marked as "copy on write," which means that
any mutations to the array will require copying the memory before making the
mutations. This preserves the guarantee that array slices are immutable values.

```
mutable_arr := @[10, 20, 30]
// No copying needed:
mutable_arr[1] = 999
>>> mutable_arr
=== @[999, 20, 30]

// This flags `mutable_arr` as "copy on write"
slice := mutable_arr[..]
>>> slice
=== [999, 20, 30]

// Since the "copy on write" flag is set, this triggers
// mutable_arr to copy its contents to a fresh data array
// before modifying:
mutable_arr[1] = -555

// The previous copy cleared the "copy on write" bit, so
// we don't need an additional copy for this operation:
mutable_arr[2] = -666

// We've successfully mutated the array:
>>> mutable_arr
=== @[-555, -666, 30]

// The immutability of `slice` still holds:
>>> slice
=== [999, 20, 30]


// The same properties hold for arrays of structs:
struct Vec{x,y:Num}
vecs := @[Vec{1,2}, Vec{3,4}]

// This sets the copy-on-write flag
slice := vecs[..]

// This write triggers a copy
vecs[1].x = 999
>>> vecs
=== @[Vec{999, 2}, Vec{3, 4}]

// The slice is still immutable
>>> slice
=== [Vec{1, 2}, Vec{3, 4}]
```

Heap-allocated arrays are also flagged as "copy on write" whenever an iteration
occurs over that array. This ensures that if an array is mutated during
iteration, there won't be any unexpected behaviors. Essentially, iteration over
an array is always an iteration over a snapshot of the array at the moment when
the iteration begins.

```
my_nums := @[10, 20, 30, 40]
for num in my_nums
    if num == 30
        // No problem:
        *my_nums = [0,0]

my_nums := @[10, 20, 30, 40]
for num in my_nums
    my_nums[3] = 999
    // No problem:
    fail if num == 999
```
