# Eleutheros

Free structures a la carte

This is an experimental POC of the "fixed free" from John De Goes' talk "Post-Free: Life After Free Monads" ([slides][post-free-slides], [video][post-free-video]).

## Example : Free monad

```scala
import eleutheros._
import iota._
import iota.QList.::

type MonadCapabilities = Pure :: Effect :: Sequence :: QNil

type FreeMonad[F[_], A] = Fixed[CopQ.OnlyL[MonadCapabilities]#Out, Nothing, F, A]
```

Eleutheros uses an unpublished branch of [iota][iota] which contains `CopQ`, a coproduct for capabilities like `Pure` and `Effect` (of type `T[_, _[_], _[_], _]`).


[iota]: https://github.com/47deg/iota
[post-free-slides]: https://www.slideshare.net/jdegoes/postfree-life-after-free-monads
[post-free-video]: https://www.youtube.com/watch?v=A-lmrvsUi2Y
