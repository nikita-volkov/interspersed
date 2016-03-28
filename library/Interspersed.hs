module Interspersed
(
  Interspersed,
  runInterspersed,
  interspersed,
)
where

import Interspersed.Prelude


newtype Interspersed m a =
  Interspersed (ReaderT (m ()) (StateT Bool m) a)
  deriving (Functor, Applicative, Alternative, Monad, MonadPlus)

instance MonadTrans Interspersed where
  lift =
    interspersed

runInterspersed :: Monad m => Interspersed m a -> m () -> m a
runInterspersed (Interspersed impl) sep =
  flip evalStateT True $
  flip runReaderT sep $
  impl

interspersed :: Applicative m => m a -> Interspersed m a
interspersed fx =
  Interspersed $
  ReaderT $
  \sep ->
    StateT $
    \first ->
      fmap (\a -> (a, False)) $
      unless first sep *> fx
