The `array-simple` package
==========================

This package tries to provide a similar interface as the `Data.Vector` and `Data.Vector.Unboxed` modules in the `vector` library, but with these advantages:

* **Fast to compile** and has **no dependencies**. The compilation times (including dependencies) on my machine are as follows:

  |                  | compile time |
  |------------------|-------------:|
  | `array`          |           8s |
  | `contiguous`     |        15s\* |
  | `vector`         |          70s | 
  | `array-simple`   |           5s |
  | `primitive`      |           7s |

  \* The compile time of the `contiguous` package drops significantly if you exclude dependencies.

* Our array type is **strict in its elements**.
* **Simple implementation**. It should be easy to check the source in the Haddocks and the Core dumps GHC generates should be much more readable.
* Our array type has **minimal space overhead** (4 words). It is now suitable for small arrays of 0-100 elements.
* We only provide functions which are **guaranteed to be effecient** as you would expect. For example the vector package provides many monadic vector construction functions, but these use lists internally unless the monad happens to be IO or ST. We believe users should explicitly use lists if they want monadic construction functions.

This does come at some costs:

* We provide only a **reduced interface**. If you are missing a function, please let us know!
* There is **no automatic fusion**. That means we do not attempt to optimize an application of several operations into a single pass.
  If you want fusion we suggest converting to a list, applying the operations to that, and converting back.
* The arrays in this package have **no cheap slicing**.

> [!NOTE]
> You should enable `optimization: 2` in your cabal.project file to get the best performance.

This package is almost ready for a first release, only final checks remain.