# Blang Semantics

This document talks about some of the Blang semantics. In other words, how
Blang translates into machine code and what it's doing under the hood.


## Lists

First of all, Lists are pointers to datastructures that look like this in C
(assuming a list with elements of type `T`):

```c
typedef struct {
    T *items;
    int64_t length; // Number of items in the list
    int64_t slack;  // Extra space that is allocated but not used
} *T_list;
```

When lists are initialized, the initial values are appended to an empty list.
Memory reallocations ensure that the list has enough capacity to hold the
required items. This code:

```blang
nums := [1, 2, x for x in 4..7] 
```

Is roughly equivalent to this C code:

```c
int64_list *nums = GC_malloc(sizeof(int64_list));
int64_list_append(nums, 1);
int64_list_append(nums, 2);
for (in64_t x = 4; x <= 7; x++)
    int64_list_append(nums, x);
```

List accesses in Blang are 1-indexed and fully bounds-checked. Accessing a list
index below 1 or above the length of the list results in a runtime failure with
an informative message. So `val := nums[i]` is roughly equivalent to the
following C code:

```c
if (i < 1 || i > nums->len)
    fail("%ld is not a valid list index", i);
int64_t val = nums->items[i-1];
```

However, 1-indexing and bounds-checking do not incur any overhead or chance of
failure when iterating over a list.

```blang
for i, x in nums
    do_thing(i, x)
```

```c
// Equivalent C code
int64_t *x_addr = nums->items;
int64_t len = nums->len;
for (int64_t i = 1; i <= len; i++, x_addr++) {
    int64_t x = *x_addr;
    do_thing(i, x);
}
```

Modifying a list while iterating over it is allowed and fully safe, but not
recommended (because it's easy to get confused and end up with bugs). Under the
hood, Blang lists maintain the following guarantees:

- Loop iterators retain a reference to the *original* list contents at the
  start of the iteration and the *original* length of the list at the start
  of iteration.

- Items may be inserted into or appended to a list during iteration, but this 
  will not change which items are iterated over. Inserting an item into the
  middle of a list will always trigger a reallocation. Appending an item to
  the end of a list may or may not trigger a reallocation.

- Items may be removed from a list during iteration, but removals will not
  change which items are iterated over, or in which order this happens.
  Removing an item from the middle of a list will always trigger a
  reallocation. Removing an item from the end of a list will never trigger a
  reallocation.

- If a new value is _assigned_ to a list entry (i.e. `list[i] = new_val`),
  the iteration will see the new value if and only if the underlying memory
  has not been reallocated.

As a rule of thumb, it's always better to avoid mutating a list during
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
