The `array-simple` package
====================

This package tries to provide a similar interface as the Data.Vector module in the `vector` library, but with these advantages:

* Fast to compile and has few dependencies. The compilation times (including dependencies) on my machine are as follows:

  |                  | compile time |
  |------------------|-------------:|
  | `array`          |           8s |
  | `contiguous`     |          15s |
  | `vector`         |          70s | 
  | `array-simple`   |           5s |
  | `primitive`      |           7s |

* Our array type is strict in its elements.
* The implementation is straightforward. It should be easy to check the source in the Haddocks and the Core dumps GHC generates should be much more readable.
* Our array type has minimal overhead (4 words). It is now suitable for small arrays of 0-100 elements.
* We only provide functions which are as effecient as you would expect. 

This does come at some costs:

* We provide only a reduced interface.
* There is no automatic fusion. That means we do not attempt to optimize an application of several operations into a single pass.
  If you want fusion we suggest converting to a list, applying the operations to that, and converting back.
* The arrays in this package are not transparently sliced. You have to manually use ArraySlice or STArraySlice which incurs 2 words extra overhead.

> [!NOTE]
> You should enable `optimization: 2` in your cabal.project file to get the best performance.

This package is not complete yet. We have the following plans:

* Move slices to their own modules and provide a more complete interface on them.
* Design some form of unboxed arrays. Hopefullly supporting compound and nested structures.