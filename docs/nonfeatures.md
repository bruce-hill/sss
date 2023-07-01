# Non-Features of SSS

These are a few features that I don't plan to ever add to SSS. This document
will explain the rationale behind not including those features. The philosophy
of SSS is prioritizing Safety, Simplicity, and Speed. The features here are
ones that violate one or more of these principles


## Non-feature 1: Polymorphism/Generics

Polymorphism is a feature that allows you to parameterize types or functions
with other types. For example, you might have a tree datastructure that holds
arbitrary datatypes and functions that operate on trees with arbitrary
datatypes:

```SSS
// This is not real code, it won't work:
def BinaryTree<T> {
    value:T
    left,right:?BinaryTree<T>
}

def contains(tree:BinaryTree<T>, obj:T)->Bool
    return yes if tree.value == obj
    if (left := tree.left) and obj < tree.value
        return contains(left)
    else if (right := tree.right) and obj > tree.value
        return contains(right)
    else
        return no

mytree := BinaryTree<Int32>{123}
fail unless mytree.contains(123)

```

It's intuitively obvious how this code *should* work, but what actually happens
is quite a bit more complicated. This goes against the core SSS value of
"Simplicity". The first thing to note is that the actual memory layout of the
`BinaryTree` struct is no longer fixed. Depending on what type `T` is, the
memory offset for `left` and `right` may be quite different. Consider these
possibilities:

T        | Offset of `value` | Offset of `left` | Offset of `right`
---------+-------------------+------------------+--------------------------
`Int`    | 0                 | 8                | 16 
`Str`    | 0                 | 16               | 24

Since the memory layout depends on what `T` is, it's not possible to generate a
single compiled version of `contains()`, we need to generate multiple versions
for every possible `T` data size that's used in the program.

Well, you might think, that's not so bad, there are only so many possible data
sizes that a program might use, so surely that's doable. However, you'd be
overlooking something particularly nasty about this code: the comparisons. The
instructions generated for a comparison are different for different datatypes.
Floating point numbers need floating point comparisons, strings need string
comparisons, and so on. That means not only does a separate version of the
function need to be made for each data *size*, but actually for each data
*type*. For a function with multiple polymorphic parameters, things can get
quite complex.

If you think this problem can be solved by only generating the compiled
functions for types you actually *call*, then you're about to run head-first
into a new problem: how to compile a standalone module that can be imported. If
we don't know at compile time how the module is going to be used, we can't
selectively build binaries that will work for only the types that a user will
use it for. After all, the user of a library may be using types that we've
never even heard of.

Now, there's one more issue with polymorphism--function values:

```SSS
my_fn := contains
```

What value should `my_fn` hold? Should it be a pointer to the instructions of
one of the instantiations of `contains()`? Which one? Should it be a wrapper
structure that holds the pointers to all the different instantiations? Should
it be a function that dynamically selects which implementation to use based on
its call site? All of these answers are pretty unsatisfying to me. The only
answer that seems self-consistent is to either require manual specification
of all parameterized types (e.g. `my_fn := contains<Int8>`) or type inference
based on what the ultimate call site (if any) is. Type inference to this extreme
degree is antithetical to SSS's design goals, since it means that you can't
actually determine the type of an expression just by looking at the expression
and the types of any variables it references.

How can we resolve this issue? Are we doomed to never have reusable code for
general-purpose datastructures or functions? The answer is that polymorphism is
actually not particularly necessary to have in a fully featured and expressive
language. There are a number of alternative techniques that solve the same
problems that polymorphism solves, but avoid the compilation and complexity
problems of polymorphism. In this regard, I take a lot of inspiration from C,
which has never had polymorphism, but has plenty of alternative ways to solve
things.

### Alternative 1: Lists and Tables

As it turns out, SSS does have exactly two types of polymorphism built into
the language: Lists and Tables. These datastructures are overwhelmingly common
and it makes sense to provide tools for handling lists and tables that hold
arbitrary types of values. In the example above, instead of using a binary tree
and a `contains()` function, you could instead just use a table:

```SSS
mydata := {123=>yes}
fail unless 123 in mydata
```

For the vast majority of cases, a solid list implementation and a solid hash
map implementation will be able to store your data and retrieve it quickly.

How does SSS resolve the issues with memory offsets? It's simple: the compiler
inlines list accesses and automatically passes memory offset information to the
hash map implementation. In other words, the compiler cheats and does
polymorphism under the hood, but only for two specific datastructures.

### Alternative 2: Pointers

Lists, hash maps, and structs might cover the vast majority of use cases, but
there are still some general purpose datastructures that fill other niches.
Let's consider quadtrees (or octtrees) as a case study. Quadtrees are meant for
efficient spatial querying. What would a reusable quadtree implementation look
like without polymorphism?

```SSS
def QuadTreeItem {
    value:@Void
    xmin,xmax,ymin,ymax:Num
}
def QuadTree {|
    subtrees:[QuadTree]
    bucket:{
        xmin,xmax,ymin,ymax:Num
        items:[QuadTreeItem]
    }
|}
    def insert(qt:QuadTree,xmin,xmax,ymin,ymax:Num,value:@Void)
        ...
    def query(qt:QuadTree,xmin,xmax,ymin,ymax:Num)->[@Void]
        ...

my_items := [@Foo{...}, @Foo{...}, @Foo{...}]
my_qt := QuadTree.new()
for item in my_qt
    my_qt.insert(
        item.x-item.width, item.x+item.width,
        item.y-item.height, item.y+item.height,
        item,
    )

for ptr in qt.query(0.0, 100.0, 0.0, 200.0)
    item := bitcast ptr as @Foo
    say "Got item: $item"
```

This is a simple solution because the data layout of the `QuadTree` and the
generated binary code for each of its methods do not depend in any way on what
the type of the contained items is.

### Alternative 3: Unique IDs

Another solution is to design the API to use opaque ID numbers instead of
passing in objects directly. For example, instead of storing a `@Void` pointer
to an object, instead store its `.id` field. That way, the quadtree
implementation can focus on the things it cares about (spatial hashing) without
worrying about the data layout of the objects in the quadtree. The API user is
in charge of translating IDs into object and vice-versa, using a hash map or
database or whatever they feel is appropriate.

### Alternative 4: Composition

Another common use of polymorphism is for having shared functionality between
objects. These objects might share some amount of common structure (certain
fields in common), which naturally leads to shared functionality. For example,
if two different objects both had a `last_modified` timestamp, you might want a
helper function to know if the object was recently modified. In some languages,
you could write a polymorphic method called `was_recently_modified(obj:T)` that
lets you know if any object with a `last_modified` field was recently modified.
The alternative to writing a polymorphic method would be to structure the code
to put common functionality into the commonly shared data (in this case, the
timestamp). So instead of `was_recently_modified(obj)`, the code would become
`obj.last_modified.is_recent()`. This lets you structure the code without code
duplication, but it also lets you write every method with specific types and
memory layouts in mind.


## Non-feature 2: Inheritance and OOP

Class-based programming in the style of Java was, in my opinion, a massive
misstep in the history of computer programming. One of the central ideas was
that you would arrange things into hierarchical class structures to allow for
reuse of code and generalized interfaces. The result of this was a massive
ballooning in code complexity, as well as a lot of indirection to hide that
complexity. SSS's ideal is to keep things simple, but OOP goes against that
principle. If you want to reuse code from different areas, use composition
instead of inheritance. Instead of having a `Character` that inherits from
`SpriteObject` and `PhysicsObject` which inherit from `GameObject` and chain
invoking super methods on each other, just save yourself a headache and create
a struct that holds different things in it:

```SSS
def Character {
    name:Str
    sprite:Sprite
    body:PhysicsBody
}
    def update(char:Character, dt:Num)
        char.body.update(dt)

    def draw(char:Character)
        char.sprite.draw_at(char.body.pos)
```

Note that SSS does have *methods*, which are nothing more than a way to
conveniently group functions into the same place as the structs they operate
on. They exist mainly to make the code tidier and to allow you to use the same
function names for different types without needing function overloading. Other
than namespacing, SSS's methods are identical to ordinary functions. SSS
methods do *not* use vtables or inheritance or dynamic dispatch. All method
resolution is done at compile time. In other words, `char.draw()` is just a
convenient shorthand for writing `Char.draw(char)`.


## Non-feature 3: Operator Overloading

Many programmers enjoy the ability to use mathematical symbols to form a sort
of abstracted notion of operations with different types. For example, if `1 +
2` is `3`, you might reason by analogy that `[1,2] + [3]` is `[1,2,3]` or `"A"
+ "B"` is `"AB"`. There are two major downsides to this idea:

### Cognitive Load

The first downside is that, although most programmers can be expected to know
basic mathematical arithmetic, and may also have a strong intuition on adding
strings or lists together, things start to break down rapidly after that point.
When users create their own operator overloads for custom types, they are
effectively creating a domain-specific language where operators have entirely
new meanings, sometimes with little association to mathematical operations.
This creates a lot of burden on readers who are not familiar with the
conventions. What happens when you subtract one list from another list? How
about multiplying a `Foo` by a floating point number? Does dividing a `Path` by
another `Path` give you a subdirectory or a type error? These questions might
be answerable, but they are often not intuitive, especially when operator
overloading is allowed for mixed types. This violates the principles of both
_Simplicity_ and _Safety_, as it requires memorizing a complex set of rules,
and may cause uncaught bugs in the case of complicated overloading rules like
Javascript's operator overloading (e.g. `1 + [] == "1"` is valid under
Javascript's operator overloading rules, but would be a compiler error in SSS).

Mixed type operator overloading (e.g. `Str + Int`) also introduces the
further complexity of how to resolve conflicting overloading rules and how to
write rules in a way that avoids code duplication while also respecting
left/right precedence.

Things get even messier *still* when you allow users to define their own
operator symbols. Never mind intuitions about `+`, absolutely no one has
intuitions about user-invented operators like `@%` or `^!`--either what they
mean or what level of precedence they have.

### Runtime Cost

Operator overloading also has an unfortunate tendency to make operations which
may be very computationally expensive *appear* to be similar in cost to
extremely fast and simple operations like integer addition. Code like `a + b`
might be near-instantaneous, or it might be making web requests to a Mars
rover. Without deep familiarity with the source code, it becomes hard to
quickly eyeball code to assess what its performance looks like. This violates
SSS's principles of both _Speed_ and _Simplicity_, as it becomes easy to
accidentally tank a program's performance and hard to reason about performance.

### Grudging Concessions

With all that said, SSS may make a few concessions to ergonomics and convention
by having a few operator overloads for unambiguous, commonplace, and
comparatively performant operations like vectorized numerical math operations
on arrays and structs. The number of operator overloads will be kept small in
order to keep the language simple, and best efforts will be made to keep the
overloads fast as well. If there is ever any doubt about what an operator
overload should mean, it will not get added to the language and an unambiguous
method call will be preferred (e.g. instead of `[a,b,c] - b`, something like
`[a,b,c].without_item(b)`).


## Non-feature 4: Private Members

In some languages, there is a notion of "private" vs. "public" members in a
struct, which is enforced by the compiler. This practice is based off of the
premise that library authors want to differentiate between contractual
guarantees and implementation-specific details that users should not depend on.
There is some merit to this idea, but in my opinion it makes debugging harder
and has the potential to introduce difficult to understand bugs. This violates
the principles of _Safety_ and _Simplicity_. For example, imagine two structs
with a hidden member that has different values in each struct. If the value is
not accessible or visible in debug printouts, then it is entirely possible to
have two values that _look_ identical in every observable way, but behave
_differently_ from each other. This adds a lot of complexity to the
programmer's reasoning and makes it nearly impossible to have a coherent system
of value semantics.

Private members also encourage people to use getter/setter anti-patterns like
`def get_x(f:Foo)->Num return f.private_x`. This sort of pattern is bad for
_Speed_: it introduces function call overhead for member access, as well as
potentially slow accessor methods. It also tends to produce a lot of
boilerplate code. A much saner alternative is to indicate that certain struct
members are not _intended_ for public use by prefixing their name with an
underscore. This preserves the ability to introspect on structs for debugging
purposes and keeps value semantics simple, while still allowing library authors
to communicate which members an end user isn't intended to care about. Any sane
programmer can see a struct member with a leading underscore and understand
that it's probably not meant for normal use.


## Non-feature 5: A Fancy Type System

SSS's philosophy with static typing is that the goal of SSS's type system is
to promote _Safety_ and _Speed_, while remaining as _Simple_ as possible. The
simplest practical type system would be something like dynamic typing, where
the user doesn't have to think much about type annotations or types at all.
However, dynamic typing sacrifices too much _Safety_ and _Speed_. SSS aims to
have a type system that is _just_ complex enough to express concepts necessary
for safety and speed, without becoming excessively complex.

There is no objective definition for what "excessively complex" means when it
comes to the tradeoff between safety and simplicity in the type system. I
personally dislike working in languages with overly complicated type systems,
so I tend to err on the side of simplicity, except in cases where it can be
demonstrated that a real-world safety problem can be mitigated or eliminated by
adding a small amount of complexity to the type system. For example, code
injection (e.g. SQL injection) is a real-world problem that can be
substantially mitigated by having a type system capable of expressing the
difference between strings that have different meanings or origins and safely
escaping values when interpolating.

However, a counterexample would be type systems that are capable of proving
bounds on the runtime or memory usage of code. There's a real safety benefit to
such type systems, but the amount of complexity added is immense. Another
counterexample would be type systems that allow for specifying abstract
interfaces which various types can implement. Interface types do not improve
safety or speed, they simply reduce boilerplate, while increasing the
complexity of the type system.

## Non-feature 6: Full Type Inference

In SSS, there are two little morsels of type inference: variable declaration
(`x := 5`) and lambda return values (`fn := (x:Int)-> x+1`). Everything else in
SSS is either annotated with a type (as in the case of function arguments and
return types) or a literal value whose type is trivial to deduce (e.g. `5` is
an `Int` and `["hi"]` is a `[Str]`). A small amount of type inference
prevents redundant noise in variable declarations like `Str str = "hello"`
(of course it's a string, the value is `"hello"`). SSS strives to hit the
happy medium between too much redundant noise and too much inference.

It is possible to use type inference much more aggressively (e.g. inferring the
return types of functions) or even to have complete type inference (no declared
types whatsoever). However, type inference can be really nasty to debug when it
goes awry. Type inference also works at cross purposes to self-documenting
code. Although it can be tedious to manually specify function argument types,
it also serves as pretty good documentation. You start a function with a
declaration of what goes in and comes out of the function, and the compiler
holds you to that declaration.

With SSS's moderate type inference, you should never have to look at how a
value is *used* in order to figure out *what* it is. And you should never have
to announce what something *is* when it's already self-evident.


## Non-feature 7: Exceptions

SSS does not plan to ever support exceptions or try/catch handling. Many of
the uses for exceptions are handled by SSS's optional types and `fail`.
Optional types provide a way to express failure and also force callers to handle
errors explicitly. Ignoring return values is also treated as a compiler error,
so callers must address any error values returned or explicitly discard them:

```SSS
def IntParseResult {|Failure|Success:Int|}
def parse_int(str:Str, base=10)->IntParseResult
    return IntParseResult.Failure if #str == 0
    n := 0
    for i in 1..#str
        c := str[i]
        if c < `0 or c > `9
          return IntParseResult.Failure
        n = n*10 + ((c as Int) - (`0 as Int))
    return IntParseResult.Success(n)

x := 0
when parse_int("Hello") is n:Success
    x += n
else
    say "Oops, failed that one"

// Alternatively, assume this succeeds and fail the program if not:
x += parse_int("hello").Success
```

When situations arise that truly require aborting a process (such as
encountering an unrecoverable bug), `fail` is a good option. Exceptions live in
a murky middle ground that attempt to address both problems
(predictable/recoverable errors and unrecoverable errors). Exceptions come with
a lot of baggage attached, however. First and foremost, there is a big drag on
performance when code relies heavily on exception handling as part of
happy-path control flow. Secondly, exceptions create a lot of cognitive load
when trying to understand code. You're left with either the task of trying to
guess which functions do or don't throw exceptions (and which exceptions), or
you're forced to annotate all thrown exceptions all the way up and down the
callstack.

SSS tries to avoid the problems of exceptions by offering a robust toolkit
for expressing different sorts of failures (either through optional values or
through tagged unions to give extra information about different kinds of
failures) and a fallback of last resort for when a program is in an
unrecoverable state. At some point in the future, SSS *may* support a limited
way to create a safe recovery point for recovering from a `fail` (inspired by Lua's
`pcall()`), but it is a low priority.


## Non-feature 8: 0-Indexed Lists

Although 0-indexing is more common than 1-indexing, SSS uses 1-indexing because
it works much better with `Range` values than 0-indexing. 1-indexing also has
some intuitive advantages when dealing with ordinal lists. The original reason
for 0-indexing is that it's a more natural representation of memory offsets: in
C, `foo[x]` is directly equivalent to `*(foo + x)`. Unfortunately, 0-indexing
is quite counter-intuitive (the fifth item is `items[4]`) and it plays very
poorly with inclusive Ranges. Taking Python as an example, Python's ranges have
the incredibly awkward property that they are *inclusive* on one side and
*exclusive* on the other side. In other words, `range(3,5)` contains the
elements `{3,4}` but not `5`. This is necessary because Python has both Ranges
and 0-indexed lists. If you want to iterate over the indices in a Python list,
you use `range(0, len(foo))`, but because of the half-openness of Python's
ranges, iterating over the same range *backwards* is extremely
counter-intuitive: `range(len(foo)-1, -1, -1)` (idiomatically, Python prefers
`reversed(range(len(foo)))` to circumvent the problem). In SSS, this is not a
problem: `3..5` simply means the items `{3,4,5}`, as you would expect. The
indices in a list are `1..#list` and backwards ranges include the same values
as forward ranges (`a..b` and `b..a by -1` include the same values).

### Modular Arithmetic

One disadvantage with 1-indexing is it makes wrapping indexes more onerous. In
C, you might say `int bucket_index = hash % buckets.len;`, but in a 1-indexed
language like Lua, it becomes the much uglier code: `local bucket_index = 1 +
((hash - 1) % #buckets)`. SSS has a simple workaround: the `mod1` operator:
`bucket_index := hash mod1 #buckets`. You can think of this as "clock
arithmetic", just like how the hours on a 12-hour clock always range from 1 to
12: 11 o'clock plus 1 hour is 12 o'clock and another hour gives 1 o'clock. In
SSS, that looks like: `11 + 1 mod1 12 == 12` and `11 + 2 mod1 == 1`.
