# Arrays

SSS arrays are implemented as compact structures with value semantics that
store a pointer to a chunk of heap-allocated memory, a length value, and a
stride value. This allows for constant-time creation of array slices, including
slices that have strides other than 1.

## Data Model

A SSS array of `Foo` items is equivalent to the following C structure:

```c
struct {
    Foo *items;
    int32_t length; // Number of items in the array
    int32_t stride; // Increment to step over the array by
    bool copy_on_write;
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
array index below 1 or above the length of the array results in a runtime
failure with an informative message. So `val := nums[i]` is roughly equivalent
to the following C code:

```c
if (i < 1 || i > nums.len)
    fail("%ld is not a valid array index. Valid indices are 1..%d", i, nums.len);
int64_t val = nums.items[(i-1)*nums.stride];
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

Modifying an array while iterating over its contents is allowed and
memory-safe, but not recommended (because it's easy to get confused and end up
with bugs). Under the hood, SSS arrays maintain the following guarantees:

- Loop iterators retain a reference to a snapshot of the *original* array
  contents at the start of the iteration and the *original* length of the array
  at the start of iteration.

- Items may be inserted into or appended to an array during iteration, but this 
  will not change which items are iterated over. Inserting an item into the
  middle of an array will always trigger a reallocation. Appending an item to
  the end of an array may or may not trigger a reallocation.

- Items may be removed from an array during iteration, but removals will not
  change which items are iterated over, or in which order this happens.
  Removing an item from the middle of an array will always trigger a
  reallocation. Removing an item from the end of an array will never trigger a
  reallocation.

- If a new value is _assigned_ to an array entry (i.e. `array[i] = new_val`),
  the iteration will see the new value if and only if the underlying memory
  has not been reallocated.

As a rule of thumb, it's always better to avoid mutating an array during
iteration, but sometimes it's unavoidable. SSS strives to mitigate the risks
in such cases.

```SSS
all_things := @[....]
// Bad! This is a bug!
for i,thing in *all_things
    if needs_removal(thing)
        // First removal is fine, but second
        // removal is off by an index
        all_things.remove(i)

// Better:
*all_things = [t for t in all_things
               unless needs_removal(t)]

```
