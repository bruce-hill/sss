# Goals
- Write much of the compiler in itself.
- Provide self-hosted compile-time transformations.
    - E.g. optimization passes, macros, type checks
- At runtime, take in a string of text and execute it as code. (`eval()`)
- Support easy DSLs
    - Parse at compile time
    - Syntax/type check at compile time
    %Spawns[
        [0:00-1:00] 10 zombies per second
        [1:00] 10 zombies
    ]
- Lua-style coroutines
- Immutable datastructures
- Optimizations based on guarantees of functional purity
- Listening for signals
