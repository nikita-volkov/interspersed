module Interspersed
(
  Interspersed,
  runInterspersed,
  interspersed,
)
where

import Interspersed.Prelude


-- |
-- An abstraction over interspersing monadic actions.
newtype Interspersed m a =
  Interspersed (ReaderT (m ()) (StateT Bool m) a)
  deriving (Functor, Applicative, Alternative, Monad, MonadPlus)

instance MonadTrans Interspersed where
  {-# INLINE lift #-}
  lift =
    interspersed

{-# INLINE runInterspersed #-}
runInterspersed :: Monad m => Interspersed m a -> m () -> m a
runInterspersed (Interspersed impl) sep =
  {-# SCC "runInterspersed" #-} 
  flip evalStateT True $
  flip runReaderT sep $
  impl

-- |
-- Lifts a monadic action. Same as 'lift'.
{-# INLINABLE interspersed #-}
interspersed :: Monad m => m a -> Interspersed m a
interspersed fx =
  {-# SCC "interspersed" #-} 
  Interspersed $
  ReaderT $
  \sep ->
    StateT $
    \first ->
      liftM (\a -> (a, False)) $
      unless first sep >> fx
