# Tables

Blang supports hash tables as a language primitive. Hash tables are one of the
most important datastructures in programming, so it's important to have them be
a core part of the language. Hash tables allow for asymptotically fast
implementations of many algorithms.

## Syntax

The syntax for tables is pretty straightforward:

```
my_table := {123=>456, 789=>10, key=>value}
>>> typeof my_table
=== {Int=Int}
```

Empty tables must specify their type:

```
empty := {:String=>Num}
```

Values can be retrieved with square bracket indexing:

```
val := my_table[123]
>>> typeof val
=== Int
```

The operation will return the value in the table corresponding to the given
key, or will skip out of the current block if the key is absent. Or, if the
current block is not a skippable block, then a runtime error will be raised.

In order to query if the key is present, a `do` block may be used:

```
do
    val := my_table[123]
    say "Got a val: $val"
else
    say "Key was missing from the table"
```

New entries can be inserted into the table by assigning to an index:

```
my_table[42] = 42
```

Iteration looks like this:

```
for entry in my_table
    say "key=$entry.key, value=$entry.value"
```

### Comprehensions

Tables can also be constructed using comprehensions, similar to Python:

```
squares := {i=>i*i for i in 1 to 10}
```

Additionally, comprehensions can be mixed with regular entries or conditional
entries:

```
t := {0=>"zero", x=>"ecks" if x != 0, i=>"$i" for i in 1 to 10, 99=>"ninety nine"}
```

## Special Behaviors

Optionally, some special behaviors may be specified at the end of a table
literal after a semicolon:

```
counter := {:String=>Int; default=0}
counter["x"] += 1

t := {1=>2; fallback=other_table, default=(key:Int)=>key+1}

nested := {:String=>{:String=>Int}; default={:String=>Int; default=0}, keep}
nested["x"]["y"] += 1
```

The currently planned special behaviors are:

- `fallback`: If a key isn't found in the table, try looking it up on the given
  fallback table before determining that it's missing.
- `default`: When a key is missing (not in the table or its fallback, if any),
  the `default` value will be used instead. If `default` is a function from key
  type to value type, it will be called with the missing key to get the value
  to use. If it is a value, it will be automatically converted to a function that
  returns that value expression (i.e. `default=blah` is shorthand for
  `default=(_:KeyType)=>blah`)
- `keep`: If `keep` is specified, default values will be kept in the table when they
  are created.

Special behaviors can be accessed (or modified) by attributes:

```
>>> typeof t.default
=== @?{Int=>Int}
>>> typeof t.fallback
=== ?(Int)->Int
>>> typeof t.keep
=== Bool
```

## Methods

Tables also provide a handful of methods:

```
// Remove an entry:
my_table.remove(key)

// Add all entries from another table:
my_table.add_all(other)

// Add any entries that aren't in this table:
my_table.add_missing(other)

// Clear all entries:
my_table.clear()
```

## Semantics

One of the most critical design decisions for hash tables is how to handle
hashing and equality for value types and pointer types. Blang opts for the
mantra of "reference equality for references, structural equality for values."
The same extends to hashing. That is, for pointer types (with `@` in the type
description), hashing and equality checks are based on the memory address
_alone_, and not the contents of the memory. For value types (everything else),
the entire memory contents of that structure are used for hashing and equality.

### Floating Point Numbers

One special case worth mentioning is that IEEE-754 floating point standards
have some _unusual_ requirements for equality of floating point numbers.
Specifically, `NaN != NaN` and `+0.0 == -0.0`. These are cases where bitwise
equality is insufficient. Blang handles these cases as follows:

- If `NaN` is used as a table key, it will produce a runtime failure.
- If `-0.0` is used as a table key, it will be normalized to `0.0`.

This means that `x == y` implies that `t[x] == t[y]`. However, it is generally
advisable to avoid using floating point numbers as table keys.

## Implementation

Tables are implemented using chained scatter tables with [Brent's variation,](https://maths-people.anu.edu.au/~brent/pd/rpb013.pdf)
essentially copying the design of Lua's `table` implementation. To summarize
briefly, each bucket in the hash table is either empty or stores an entry and
an optional pointer to the next bucket to check if the entry isn't what you
were looking for. As an example, this shows a table where `A` hashes to `0`,
`X` hashes to `1`, and `B` also hashes to `0`, causing a collision:

```
   0   1   2
  ┌─┬─┬─┬─┬─┬─┬
  │A│*│X│ │B│ │...
  └─┴┼┴─┴─┴▲┴─┴
     └─────┘
  Next in chain
```

During lookup, the process is straightforward: compute the hash of the key,
jump to the corresponding bucket. If it matches the key, you're done. If not,
check if there is a next bucket in the chain and check that. Repeat until there
are no more chained buckets or a match is found. Every item in the chain
represents a hash collision, so generally speaking, chains won't be very long.

This approach is highly compact: everything is stored in a single array of
buckets, there aren't any additional heap allocations.

Brent's variation imposes the following invariant: if an element hashes to a
particular bucket, it _must_ be put in the chain that starts in that bucket.
The simple case is when you want to insert an element, but there is already an
element with the same hash value in the table. In this case, the new element is
inserted in any empty bucket and appended to the chain that starts in its
"home" bucket:

```
 Initial state:

   0   1   2   3
  ┌─┬─┬─┬─┬─┬─┬─┬─┬
  │A│*│X│ │B│ │ │ │...
  └─┴┼┴─┴─┴▲┴─┴─┴─┴
     └─────┘

 Insert C with hash(C) == 0:

  ┌ Bucket 0 is occupied, so find any empty
  │ bucket and insert C there, appending it
  │ to the chain of elements that hash to 0
  ▼
  0   1   2   3
 ┌─┬─┬─┬─┬─┬─┬─┬─┬
 │A│*│X│ │B│*│C│ │
 └─┴┼┴─┴─┴▲┴┼┴▲┴─┴
    └─────┘ └─┘
```

A more complicated case arises if we want to insert an element that hashes to
a bucket that is somewhere downstream in a chain that starts somewhere else.
In order to preserve the invariant, we must displace the downstream element
so that the bucket can serve as the head of a chain of elements that actually
hash to that bucket's index. For example, if we inserted an element `Y` that
hashes to `2` into the original example table, we would need to displace `B`
to any free bucket and update `A`'s "next in chain" pointer, so that `Y` can
go into bucket `2` and ensure that it can be found quickly:


```
 Initial state:

   0   1   2   3
  ┌─┬─┬─┬─┬─┬─┬─┬─┬
  │A│*│X│ │B│ │ │ │...
  └─┴┼┴─┴─┴▲┴─┴─┴─┴
     └─────┘

 Insert Y with hash(Y) == 2:

First displace B (which doesn't hash to 2)
          ┌───┐
 ┌─┬─┬─┬─┬┼┬─┬▼┬─┬
 │A│*│X│ │.│ │B│ │
 └─┴─┴─┴─┴─┴─┴─┴─┴
  0   1   2   3

 ┌─┬─┬─┬─┬─┬─┬─┬─┬
 │A│*│X│ │ │ │B│ │
 └─┴┼┴─┴─┴─┴─┴▲┴─┴
    └─────────┘
Update the chain pointer

Insert new element at its "home" bucket
          │
 ┌─┬─┬─┬─┬▼┬─┬─┬─┬
 │A│*│X│ │Y│ │B│ │
 └─┴┼┴─┴─┴─┴─┴▲┴─┴
    └─────────┘
```

This approach requires a little bit of extra bookkeeping work at insertion time
to preserve the invariant. However, the result is a hash table that is fairly
simple to implement, extremely compact, and performs extremely fast lookups,
even when the table is at 100% occupancy.
