The `vector-simple` package
====================

This package tries to provide a similar interface as the Data.Vector and Data.Vector.Mutable modules in the `vector` library, but with these advantages:

* It is much faster to build from scratch.
* Our vector type is strict in its elements. This is ensured by data-elevator, so GHC will know that any value you read from our vectors is already fully evaluated, saving an eval check.
* The implementation is much more straightforward. It should be easy to check the source in the Haddocks and the Core dumps GHC generates should be much more readable.
* Our vector type has less overhead (the total size is: 1 word for the header, 1 word for the length, and then 1 word for each item). It is now suitable for small arrays of 0-100 elements.
* We only provide functions which are as effecient as you would expect. 

This does come at some costs:

* We provide only a reduced interface.
* There is no automatic fusion. That means we do not attempt to optimize an application of several operations into a single pass.
  If you want fusion we suggest converting to a list, applying the operations to that, and converting back.
* The vectors in this package are not transparently sliced. You have to manually use VectorSlice or STVectorSlice which has 2 words extra overhead.

Note: you should enable `optimization: 2` in your cabal.project file to get the best performance.

This package is not complete yet. We have the following plans:

* Move slices to their own modules and provide a more complete interface on them.
* Design some form of unboxed vectors. Hopefullly supporting compound and nested structures.