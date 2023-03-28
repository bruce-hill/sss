# Blang Semantics

This document talks about some of the Blang semantics. In other words, how
Blang translates into machine code and what it's doing under the hood.


## Arrays

First of all, Arrays are datastructures that look like this in C (assuming an
array with elements of type `T`):

```c
typedef struct {
    T *items;
    int32_t length; // Number of items in the array
    int32_t stride;  // Increment to step over the array by
} T_Array;
```

When arrays are initialized, the initial values are appended to an empty array.
Memory reallocations ensure that the array has enough capacity to hold the
required items. This code:

```blang
nums := [1, 2, x for x in 4..7] 
```

Is roughly equivalent to this C code:

```c
int64_Array nums = {0};
int64_array_append(nums, 1);
int64_array_append(nums, 2);
for (in64_t x = 4; x <= 7; x++)
    int64_array_append(nums, x);
```

Array accesses in Blang are 1-indexed and fully bounds-checked. Accessing an
array index below 1 or above the length of the array results in either skipping
past the rest of the block (if inside a loop or `do` block), or a runtime
failure with an informative message. So `val := nums[i]` is roughly equivalent
to the following C code:

```c
if (i < 1 || i > nums->len)
    fail("%ld is not a valid array index", i);
int64_t val = nums->items[i-1];
```

However, 1-indexing and bounds-checking do not incur any overhead or chance of
failure when iterating over an array.

```blang
for i, x in nums
    do_thing(i, x)
```

```c
// Equivalent C code
int64_t *items = nums->items;
int64_t len = nums->len;
for (int64_t i = 1; i <= len; i++) {
    int64_t x = items[i];
    do_thing(i, x);
}
```

Modifying an array while iterating over it is allowed and fully safe, but not
recommended (because it's easy to get confused and end up with bugs). Under the
hood, Blang arrays maintain the following guarantees:

- Loop iterators retain a reference to the *original* array contents at the
  start of the iteration and the *original* length of the array at the start
  of iteration.

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
iteration, but sometimes it's unavoidable. Blang strives to mitigate the risks
in such cases.

```blang
// Bad! This is a bug!
for i,thing in all_things
    if needs_removal(thing)
        // First removal is fine, but second
        // removal is off by an index
        all_things.remove(i)

// Better:
all_things = [t for t in all_things
              unless needs_removal(t)]

```
