# Features

This article is about some neat features of SSS. Some of them are taken
from other languages, some of them are original.

## Value Semantics

SSS offers well-defined value semantics for all types in the language. What
does this mean? It means that for every type in the language except for pointer
types, the following axioms hold:

- **Axiom 1: Immutability** if `a == b`, then it will always be the case that
  `a == b`, unless a new value is assigned to either variable.
- **Axiom 2: Hashing** if `a == b` and `table` is a table, then `table[a] ==
  table[b]`.
- **Axiom 3: Memoization** if `a == b` and `fn` is a function that has
  memoization, then `fn(a) == fn(b)`, provided that `fn` has an unlimited cache
  size or the return value for `a` is still in the cache when `fn(b)` is
  called.
- **Axiom 4: Arrays** if `a` and `b` are both arrays of the same type, then `a
  == b` if and only if `a` and `b` are the same length and `a[i] == b[i]` for
  all indices `i`.
- **Axiom 5: Structs** if `a` and `b` are both structs of the same type, then
  `a == b` if and only if `a.member == b.member` for all struct members.
- **Axiom 6: Tagged Unions** if `a` and `b` are both tagged unions of the same
  type, then `a == b` if and only if both have the same tag, and the same
  tagged value (if any).
- **Axiom 7: Tables** if `a` and `b` are both tables of the same type, then `a
  == b` if and only if `a` and `b` have the same keys and `a[key] == b[key]`
  for all keys.
- **Axiom 8: Ordered Comparisons** for all types except tables (which do not
  support ordered comparisons), all statements that are true for `==` are also
  true for ordered comparison operators (`<`, `<=`, `>`, `>=`). Ordered
  comparisons for structs, tagged unions, and arrays are performed elementwise,
  finding the first non-equal element, if any and returning the comparison for
  that element. For tagged unions, tagged values are only compared if both
  tagged unions have the same tag.

These axioms are meant to empower you to be able to reason about your programs
more easily. Each one of these statements is a promise from the language to
you, the programmer, which will allow you to more easily write correct programs.

### Pointers

Pointers, unlike the types mentioned above, use reference semantics. That is to
say, pointers are a numeric value that represents a memory address, and
comparisons between pointers or hashing pointers means comparing or hashing the
_address_, not the content of the memory that resides there. Two pointers are
equal if and only if they refer to the same location in memory. This means that
if `a` and `b` point to the same location in memory, assigning a new value to
`*a` is equivalent to assigning a new value to `*b`. If you want to check for
structural equality between two pointers, you can access the underlying values
by dereferencing the pointers: `*a == *b`.

### Floating Point NaN Values

The one exception to the above rules is that floating point numbers (`Num` and
`Num32`) comply with the IEEE-754 specification for handling NaN (not-a-number)
values, which require that if `x` is NaN, then all comparisons with `x` must
return `no`. However, hashing and memoization work for NaN values:
`table[Num.NaN] == table[Num.NaN]` and `fn(Num.NaN) == fn(Num.NaN)`


## Language-level Function Memoization

One of the most important upsides of the value semantics mentioned above is
that SSS supports storing any type (other than `Void` or `Abort`) as a table
key. This makes it possible for the language itself to provide automatic
memoization of functions if desired:

```
def fibonacci(n:Int; cached)->Int
    if n <= 1
        return 1
    return
        return fibonacci(n-2) + fibonacci(n-1)
```

The example above uses `; cached` to signal to the compiler that the function
return values should be cached in a table. This is functionally equivalent to:

```
global fibonacci_cache := @{{Int}=>Int}
def fibonacci(n:Int; cached)->Int
    key := {n}
    if key not in fibonacci_cache
        if n <= 1
            fibonacci_cache[key] = 1
        return
            fibonacci_cache[key] = fibonacci(n-2) + fibonacci(n-1)
    return fibonacci_cache[key]
```

I believe that function memoization is one of the most important techniques in
writing performant code. However, because many languages lack consistent value
semantics, it is often not possible to memoize many functions in many
languages, or requires special case logic to work around the language
limitations. SSS aims to make it trivially easy (just seven keystrokes:
`;cached`) to asymptotically improve a function's runtime performance in a way
that strives to make it easy to write correct code handles cache invalidation
correctly. When pure functions operate over immutable values, caches never
need to be invalidated.

Since there is always a tradeoff between memory and speed, sometimes it is
desirable to give a function a cache, but limit the maximum size of that cache.
In SSS, this is achieved by adding `; cache_size=N`, where `N` is a constant
integer value. When a cache size is specified and the cache reaches that size,
a value is evicted from the cache to make space for new cache entries without
exceeding the maximum size. The current implementation evicts a _random_ entry
from the cache, since random eviction is time-efficient and does not require
any additional memory overhead.


## Better Loops

Loops are one of the most fundamental features of imperative programming
languages, but there's still some room for improvement. SSS supports two
types of loops: `for` loops and `while` loops (with `repeat` as syntactic
sugar for `while 1`). `while` loops follow the standard pattern of checking
a condition:

```python
while i < 10
    i += 1
```

And `for` loops iterate over values in an iterable container or numeric range:

```python
for item in list
    say "$item"

for i in 1..5
    say "$i"
```

`for` loops also have an optional extra parameter used for an index:

```python
for i,item in list
    say "$i: $item"

for i,n in 99..150
    say "#$i = $n"
```

Most languages with loops also offer a `continue` and `break` statement for
altering control flow. And a select few languages also offer
breaking/continuing encompassing loops. For clarity and brevity, SSS uses
`skip` and `stop` for this functionality and supports skipping/stopping
loops by referencing either a loop variable or the type of loop:

```python
while i < 10
    if i == 7
        stop

    if i mod 2 == 0
        skip

for outer in things
    for inner in other_things
        ...
        stop outer if inner + outer > 100
        ...
```

So far, all of these features exist in other languages, but the next feature is
something I believe is original to SSS: `between` blocks. A really common
behavior in programming tasks is doing something between iterations. In SSS,
it looks like this:

```python
str := ""
for num in nums
    skip if num < 0
    str += "$num"
between
    str += ", "

repeat
    data := poll_server()
    if new_messages(data)
        notify_user(data)
between
    wait(10)
```

The alternative in other programming languages is to track which iteration the
loop is on and have a conditional branch to selectively execute "between" logic
at the front of each loop iteration only when not on the first iteration, which
is ugly, cumbersome, and may result in unnecessary condition-checking overhead:

```python
s = ""
first_loop = true
for num in nums
    if num < 0: continue
    if first_loop
        first_loop = false
    else
        s += ", "
    s += str(num)
```

SSS loops also support a `first` block, which only executes on the first
iteration:

```python
best := 0
for num in nums first
    best = num
between
    if num > best
        best = num
```

## Vectorized Math Operations

Inspired by languages like like APL and Octave, SSS, supports automatically
extending math operations to structs and arrays that hold numeric values. All
operations are pairwise and do not require operator overloading.

For example, adding two 2D vectors:

```SSS
def Vec2{x,y:Num}

v1 := Vec2{2, 3}
v2 := Vec2{10, 20}
>>> v1 + v2
=== Vec2{12, 23}
```

Operations are also automatically defined for scalar operations on numeric containers:

```SSS
v1 := Vec2{2, 3}
>>> v1 * 10
=== Vec2{20, 30}

nums := [1, 2, 3, 4]
>>> nums * 10
=== [10, 20, 30, 40]
```

## Seamless Slicing of Arrays-of-Structs

When dealing with arrays of structs, SSS supports easy (and constant-time)
creation of slices that contain one member of a struct:

```
def Enemy{id:Int, name:Str}

enemies := @[
  Enemy{123, "Evil Ed"},
  Enemy{456, "Bad Bob"},
]

>>> enemies.id
=== [123, 456]
>>> enemies.name
=== ["Evil Ed", "Bad Bob"]
```

## Units of Measure

Inspired by [Graydon Hoare's blogpost "What's
next?"](https://graydon2.dreamwidth.org/253769.html) as well as the
implementation of [F#'s Units of
Measure](https://docs.microsoft.com/en-us/dotnet/fsharp/language-reference/units-of-measure),
SSS supports typed units of measure with typechecking and automatic
conversions between units.

```python
def 1<km> := 1_000<m>
def 100<cm> := 1<m>
def 10<mm> := 1<cm>

>>> 1<m> + 1<cm>
=== 1.01<m>

>>> 10.0 * 2.5<m>
=== 25.0<m>

def 1<min> := 60<s>
def 1<hr> := 60<min>

// 1<km> + 1<hr> // Type error!
// 12 + 1<hr> // Type error!

def 1<inch> := 2.54<cm>
def 1<ft> := 12<inch>
def 1<yd> := 3<ft>
def 1<mi> := 5_280<ft>

>>> 10<mi> / 2<hr>
=== 2.2352<m/s>
>>> 1<mi>/1<km>
=== 1.60934
```

Type checking and unit conversions are all performed *at compile time* relative
to irreducible base units (i.e. those not defined in relation to other units),
and all math occurs as double precision floating point operations in base
units. In other words, there is no run-time overhead when performing unitful
calculations.

Units of measure can also be applied to structs, such as vectors:

```python
def Vec2{x,y:Num}

pos := Vec2{0,0}<m>
vel := Vec2{5,3}<m/s>
pos += 2<s>*vel
```

## Minimally Intrusive Optional Types

Optional types in many programming languages are either verbose and annoying to
use, overengineered and complex, or totally absent. SSS takes an approach to
values which may or may not exist that is simple and pragmatic. `!Foo` is used
to indicate the absence of a `Foo` value that would reside on the heap if it
existed. `@Foo` is used for values that are guaranteed to exist, and `?Foo`
for values that may or may not exist.

```python
def Vec{x,y:Num}

def needs_value(v:@Vec)
    say "v is guaranteed to be a non-null pointer"
    say "so accessing fields is safe:"
    say "$(v.x)"

needs_value(@Vec{1,2}) // okay
needs_value(!Vec) // compiler error
```

If you have a pointer that may or may not be null, there are a couple of
options available to get to a point where it's safe to pass it to a function
that requires a non-null pointer.

For example, consider this situation:

```python
vec := if consition
    @Vec{1,2}
else
    !Vec

>>> typeof vec
=== ?Vec

// Want to call: needs_value(vec)
```

### The Postfix Exclamation Operator

The first, and simplest situation is to tell SSS that you want to trigger a 
runtime failure if the pointer is null. The compiler can assume that if the
program execution is still going, the expression must not have been null.

```
>>> typeof vec
=== ?Vec
>>> typeof vec!
=== @Vec

// If vec is null, this will trigger a runtime failure with an informative
// message, otherwise it will succeed:
needs_value(vec!)
```

### Fallback Values with "or"

SSS also lets you provide an alternative value to use in the event that the
pointer is null using `or`:

```
>>> typeof vec
=== ?Vec
>>> typeof (vec or @Vec{0,0})
=== @Vec

needs_value(vec or @Vec{0,0})
```

### Use Control Flow with "or"

The last way to handle null values is to interrupt execution with `return`,
`fail`, `stop`, or `skip`. For example, in the context of a loop, we know
that `vec or skip` will evaluate to either a non-null pointer, or the current
loop iteration will be skipped. Therefore, if the rest of the loop iteration
is executed, we know that the value must have been non-null.

```python
maybe_vecs := [@Vec{1,2}, !Vec, @Vec{2,3}]
for vec in maybe_vecs
    >>> typeof vec
    === ?Vec
    >>> typeof (vec or skip)
    === @Vec
    needs_value(vec or skip)

def get_x(v:?Vec)->Num
    v_nonnull := v or return 0
    return v_nonnull.x
```

## String Interpolation

Like many modern programming languages, SSS supports string interpolation,
which makes it easy to insert values into strings:

```python
my_name := "Bob"
say "Hi, my name is $my_name and my favorite number is $(Int.random())"
```

When converting a value to a string for an interpolation, SSS automatically
generates the code to format the value, including all its fields and data,
in a canonical format that is nearly identical to SSS code.

## DSLs

SSS supports safe-by-design strings representing non-SSS code. An extremely
common source of security vulnerabilities occurs when intermixing code (e.g.
SQL code or HTML code) with user-controlled inputs. The result is code
injection attacks. SSS addresses this problem by providing DSL strings (DSL =
Domain Specific Language). DSL strings come with compile-time type safety to
prevent accidentally mixing up different types of strings and automatic string
escaping when performing string interpolations.

### Easy Language Embedding

SSS allows for writing strings that contain code using DSL strings, which
self-document which domain-specific language the string uses, as well as
providing multiple different string delimiters to make it easy to avoid
[leaning toothpick
syndrome](https://en.wikipedia.org/wiki/Leaning_toothpick_syndrome). DSL
strings are prefixed by `$` and an identifier, followed by a pair of
delimiters, like `$Foo"quotes"`, `$Foo[brackets]`, `$Foo/slashes/`,
`$Foo;semicolons;`, `$Foo{braces}`, and so on. Additionally, `>` and `:` can be
used to indicate the string continues till the end of the line. Different
domain-specific languages have different values that need to be escaped, so
it's important to have options for how to most conveniently represent whatever
you need to.

```
pat := $Regex/[0-9]+|['"]+/
json := $JSON!{"key"=[1,2,3]}!
sss := $SSS`foo := $DSL[...]`
shell := $Shell> ls $HOME
```

SSS's multi-line strings use indentation to delimit string boundaries, and
DSL strings can have the same format. Strings end where indented regions end
(discarding trailing blank lines). Custom quotation marks are not needed to
avoid ambiguity:

```
json := $JSON:
    {
        "key"="value",
        "foo"=99
    }

html := $HTML:
    <ul>
        <li>...</li>
    </ul>
```

### Sanitizing Inputs

As an example, let's consider SQL injection. A common security vulnerability in
web application occurs when a programmer naively attempts to compose an SQL query
out of hand-written SQL components and user-supplied strings. For example, using
the Python sqlite library:

```python
# Never do this -- insecure!
symbol = get_requested_symbol()
sql.execute("SELECT * FROM stocks WHERE symbol = '%s'" % symbol)
```

The problem here is that if `symbol` is supplied by an attacker, or just not
properly escaped by the programmer, [bad things can
occur.](https://xkcd.com/327/) To make matters worse, in this Python library,
there is no way to tell that anything has gone wrong until it's too late,
because there is no differentiation between "safe" SQL strings authored by the
programmer and "unsafe" strings from elsewhere in the program.

In SSS, there is a much better solution for this problem: DSL strings.

```python
def s:Str as $SQL
    return bitcast ("'" ++ (str.replace("'", "''")) ++ "'") as $SQL

symbol := get_requested_symbol()
query := $SQL"SELECT * FROM stocks WHERE symbol = $symbol"
sql_execute(query)
```

DSL strings offer three important benefits: firstly, DSL strings are an easy way
to write code within code. The `$Name` prefix clearly documents what type of string you're dealing
with, and there is support for multiple different delimiters, whichever is most
convenient for your domain. Nobody wants to sort through code with [leaning
toothpick syndrome](https://en.wikipedia.org/wiki/Leaning_toothpick_syndrome),
so instead, DSL strings are delimited by matching square brackets with _no
escape characters_ other than `$` for interpolation. Alternatively, other
delimiters are available for use, including `/slashes/`, `|pipes|`,
`;semicolons;`, `,commas,`, `'single quotes'`, `"double quotes"`, `` `backticks` ``,
or `{curly braces}`. Different domain-specific languages have different values
that need to be escaped, so it's important to have options for how to most
conveniently represent whatever you need to.

Secondly, it will be a compile-time type error if the programmer attempts to
pass an unsafe string to a function that expects an SQL query string:

```python
s := get_unsafe_string()
sql_execute(s) // <-- type error
```

And finally, DSL string interpolation *automatically* escapes values, using a
user-defined escaping function. Doing the safe thing (escaping values) becomes
the easy and automatic thing to do.

```python
malicious := "xxx'; drop table users; --"
query := $SQL"SELECT * FROM users WHERE name = $malicious"
say "$query"
// prints: SELECT * FROM users WHERE name = 'xxx''; drop table users; --'
```

DSL strings also allow escaping values besides strings, which can be useful in
cases like escaping lists of filenames for shell code:

```python
def str:Str as $Shell
    return ("'" + (str | replace("'", "'\"'\"'")) + "'"):Shell

def strings:[Str] as $Shell
    ret := $Shell""
    for str in strings
        ret += $Shell"$str"
    between ret += $Shell" "
    return ret

files := ["file.txt", "`rm -f $HOME`", "isn't safe"]
dest := "/tmp"
cmd := $Shell> cp @files @dest
say "$cmd"
// prints: cp 'file.txt' '`rm -f $HOME`' 'isn'"'"'t safe' /tmp
```

### Preventing Data Leaks

DSLs can also be used to guard against sensitive information being revealed
accidentally.

```python
def User {name:Str, password_hash:Str, credit_card:Str}

def check_credentials(users:{Str=User}, username:Str, password:Password)->Bool
    user := users[username] or return no
    if hash_password(password) == user.password_hash
        return yes
    else
        // Oops, accidental potential leak of sensitive information:
        log("Failed login attempt for $user")
        return no
```

In the code above, the call to `log("Failed login attempt for $user")` is
exactly the sort of innocuous-looking code where a programmer might have
accidentally typed `user` instead of `username`, and as a result, a logfile on
disk might end up containing all kinds of sensitive information like credit
card numbers or password hashes:

```
[log] Failed login attempt for User{name=Roland, password_hash=12345, credit_card=12345678}
```

One way to avoid this problem is to use custom DSL strings for sensitive data,
which defines a custom `tostring()` implementation that obscures any private data:

```python
def s:Str as $Sensitive
    return s:SensitiveString
def sensitive:$Sensitive as Str
    return "******"

def User {name:Str, password_hash:Sensitive, credit_card:Sensitive}
```

If `User` is defined in this way, the accidental log line will print a much
more benign log message:

```
[log] Failed login attempt for User{name="Roland", password_hash=******, credit_card=******}
```

Even if the programmer prints `log("User: $username hash:
$(user.password_hash)")` directly, the resulting string will have safely
redacted values.

## Security

Some ideas in SSS were inspired by Cristina Cifuentes' talk [What Is a Secure
Programming Language?](https://www.youtube.com/watch?v=dhoP-dyIr54) In the
talk, Cifuentes points out that roughly half of all exploited vulnerabilities
discovered in the National Vulnerability Database come down to buffer errors,
code injection, or accidental data leaks.

## Percentages

SSS allows you to use percentages as a form of number. Percentages are a
commonly used way to express and think about fractions of a whole, but nearly
all mainstream languages force programmers to use fractional real numbers (e.g.
`.02` instead of `2%`) and then multiply by 100 every time they need to be
represented visually as a percent. This is serviceable, but somewhat tedious
and unintuitive, and occasionally leads to mistakes. SSS allows for
percentage literals like `2%`, which internally compiles to the floating point
representation equivalent to `.02` but comes with two added advantages: it
automatically prints as "2%" rather than "0.02", and it comes with a few type
safety guarantees. The compiler allows normal math operations between
percentages (adding, multiplying, etc.) but treats addition and subtraction
between percentages and non-percentages as a type error (e.g. `5% + 10.0`).
Multiplying percentages by numbers or units of measure produces something of
the same type, as you would expect, so `50%*10.0<km> + 2.0<km> == 7.0<km>`

To be frank, percentages are a bit of a gimmicky language feature, but they
were easy to add and interesting as an experiment.
