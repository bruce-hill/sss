# Arrays

SSS arrays are implemented as compact structures with value semantics that
store a pointer to a chunk of heap-allocated memory, a length value, and a
stride value. This allows for constant-time creation of array slices, including
slices that have strides other than 1.

## Data Model

A SSS array of `Foo` items is equivalent to the following C structure:

```c
struct {
    // A pointer to the array items:
    Foo *data;
    // The length of the array:
    int64_t length:42;
    // Number of extra items that can fit after the end of the array:
    uint8_t free:4;
    // Whether to the array must be copied before writing:
    bool copy_on_write:1;
    // Whether or not the item type holds references to memory:
    bool atomic:1;
    // The address difference between items (can be negative):
    int16_t stride:16;
}
```

When arrays are initialized, the initial values are appended to an empty array.
Memory reallocations ensure that the array has enough capacity to hold the
required items. This code:

```SSS
nums := [1, 2, x for x in 4..7] 
```

Is roughly equivalent to this C code:

```c
array_t nums = {0};
array_append(&nums, 1);
array_append(&nums, 2);
for (in64_t x = 4; x <= 7; x++)
    array_append(&nums, x);
```


## Indexing

Array accesses in SSS are 1-indexed and fully bounds-checked. Accessing an
array index of zero or above the length of the array results in a runtime
failure with an informative message.

Negative array indices are treated as back-indexed. In other words, -1 refers
to the last item, -2 refers to the second-to-last, and so on. Negative indices
below the negative length of the array will cause a runtime error.

So `val := nums[i]` is roughly equivalent to the following C code:

```c
int64_t effective_index = i >= 0 ? i : (nums.len + i + 1);
if (effective_index < 1 || effective_index > nums.len)
    fail("%ld is not a valid array index. Valid indices are +/-1..%d", i, nums.len);
int64_t val = nums.items[(effective_index-1)*nums.stride];
```

However, 1-indexing and bounds-checking do not incur any overhead or chance of
failure when iterating over an array.

```SSS
for i, x in nums
    do_thing(i, x)
```

```c
// Equivalent C code
for (int64_t i = 1; i <= nums.len; i++) {
    int64_t x = nums.items[(i-1)*nums.stride]; // No bounds check needed
    do_thing(i, x);
}
```

## Mutability

SSS array values are immutable, however, heap-allocated array structures are
mutable. Dereferencing an array pointer or taking a slice of an array pointer
causes a copy-on-write bit to be set on the array pointer.

```
arr := [1,2,3]
arr[1] = 999 // Illegal

heap_arr := @[1,2,3]
tmp := heap_arr[..]
heap_arr[1] = 999
>>> heap_arr
=== @[999, 2, 3]
>>> tmp
=== [1, 2, 3]
```

Whenever an array mutation occurs, the copy-on-write bit is checked, and if
set, a copy of the array data is created before mutating and the copy-on-write
bit is cleared. This ensures that mutating mutable chunks of memory does not
violate the immutability guarantees of array values, while also avoiding the
need to create many copies of arrays every time an array is dereferenced or
sliced.


### Iteration and Mutability

To avoid bugs caused by mutation during iteration, SSS does not allow iterating
over mutable arrays. However, it is very simple to get an immutable value
"snapshot" of a mutable array by dereferencing or slicing it. Here is an example:

```sss
my_arr := @[1,2,3]

// Compiler error:
// for x in my_arr

// Safe:
for x in my_arr[]
    say "$x"
```

Because arrays work as copy-on-write, this operation does not create a copy of
the mutable array unless the array is mutated.

In general, it's much less bug-prone to avoid appending or removing from arrays
during iteration and instead use array comprehensions:

```SSS
all_things := @[....]
// Bad! This is a bug!
for i,thing in all_things[]
    thing.update()
    if needs_removal(thing)
        // First removal is fine, but second
        // removal is off by an index
        all_things.remove(i)

// Better:
for thing in all_things[]
    thing.update()
all_things[] = [t for t in all_things[]
                unless needs_removal(t)]

```
