# Peregrine

Peregrine is an embedded domain specific language (EDSL) for implementing automated trading strategies. For more information on its design and the problems it is trying to solve, please see [Benefits.md](Benefits.md).

This codebase currently presents a proof-of-concept implementation - it contains a Haskell EDSL, code generator to C and a couple of data source (aka protocol) implementations. A couple of test signals are implemented in [app/Main.hs](app/Main.hs) and [src/Language/Peregrine/DSL.hs](src/Language/Peregrine/DSL.hs), for instance a rolling covariance is implemented purely in terms of signals.

Currently [app/Main.hs](app/Main.hs) can be run assuming there are `lz4` compressed files in a `data/TAQ/` folder (please see [https://www.tmxwebstore.com/products/trades-and-quotes-daily](https://www.tmxwebstore.com/products/trades-and-quotes-daily) for specifications and sample files). The most interesting thing for an interested hacker to do would probably be to write some sample programs using peregrine as a library and inspect the generated code.

## Code Organization

A rough layout of the code is as follows:
```
src

- Language
  - C # A utilities and a DSL for generating C/C++
  - Peregrine # Peregrine definitions
    - AST.hs # Definitions of core peregrine primitives
    - DSL.hs # Functions to make writing peregrine more natural
    - Compiler.hs # Code generator
    - Run.hs # Utility functions to compile and run a peregrine program

- Protocol # 'Protocol' definitions + instructions how to generate C parsers

- Main.hs # Main entry point for compiling a PoC and running it
- Utils.hs # Utility functions which are probably in somebody else's prelude replacement but it was just easier to write them
```

## Core Concepts
Peregrine is designed to replace callback-based trading systems by abstracting state and dependency management. The basic concept in Peregrine is a signal - this is a piece of state which is updated whenever the callback fires. For instance, a very simple signal could be the price included in a 'bid' message. Whenever a new message for that arrives, the 'bid' price is updated, and any signals which were constructed out of that (and depend on the 'bid' price) are also updated.

The other primitives in Peregrine are based on that core concept. For instance, `last x` retains the value of `x` from the previous tick (its value is undefined until the first tick). For instance if `bid` took the values `[ 31.05, 31.06, 31.02 ]`, `last bid` would be `[ undefined, 31.05, 31.06 ]`

The least intuitive primitive is probably `groupBy`. `groupBy x y` corrals the value of `y` based on `x`. This is useful when you want to segment information by `x`, for instance by symbol. `groupBy symbol bidprice` would essentially create a data structure which maps symbols to their latest bidprice. Anything within the group by block will behave as though it is only updated when the bidprice is updated for a particular symbol. So for instance, the following pseudocode will compute the rolling VWAP for every symbol (and store it in a mapping from symbol to its VWAP):
```
vwap group = groupBy group $ do
  px <- taqTradePrice
  sz <- taqTradeSize
  value  <- sum =<< px *. sz
  volume <- sum sz
  return (value / volume)
```

Other primitives:
- `project`: Grab a field within a message defined by a protocol, e.g. the price field of a bid message would be essentially `project bid "price"` (abbreviated herein as `bid.price`). This is the basic building block used to construct a signal.
- `zipWith`: Merge the dependency graphs of two signals, updating the output when either input fires. The output will be the specified binary operation applied to the two most recent values of the inputs.
- `merge`: Merge the dependency graphs of two signals, updating the output when either input fires. The output will be the latest value of the two inputs. This is useful for instance to reify a particular field across multiple message types, e.g. `trade.symbol` and `quote.symbol`.
- `map`: Apply a pure function to a signal.
- `fold`: Apply a pure function to a signal and an implicit accumulator. For instance a rolling sum would essentially be written `fold (+) signal`.
- `window`: Apply a pure function to a signal and a rolling window accumulator, for instance the total trade value over the last 100ms. The rolling window is defined using a maximum time difference between the latest value and the oldest value. Internally implemented as a deque which maintains `O(1)` `push`, `pop` and `accumulate` (see [src/Language/C/Lib.hs](src/Language/C/Lib.hs) for implementation details)
- `guard`: Only send updates downstream if the argument is satified.
- `last`: The value of the signal before the current tick.
- `restrict`: restrict x to fire only when y fires. Kind of like the inverse operation of merge.
- `observe`: Print a value every time it is updated. Mainly for debugging but the machinery for this can be expanded to create side effects.
- `summary`: Summarize (print) a value at the end of the program run.
