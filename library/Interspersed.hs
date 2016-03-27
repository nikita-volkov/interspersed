module Interspersed
(
  Interspersed,
  runInterspersed,
  interspersed,
)
where

import Interspersed.Prelude


newtype Interspersed m a =
  Interspersed (m () -> (m a, Bool))
  deriving (Functor)

instance Applicative m => Applicative (Interspersed m) where
  pure a =
    Interspersed (const (pure a, False))
  (<*>) (Interspersed impl1) (Interspersed impl2) =
    Interspersed impl3
    where
      impl3 separator =
        case impl1 separator of
          (m1, nonEmpty1) ->
            case impl2 separator of
              (m2, nonEmpty2) ->
                if nonEmpty1 && nonEmpty2
                  then (liftA2 ($) m1 (separator *> m2), True)
                  else (liftA2 ($) m1 m2, nonEmpty1 || nonEmpty2)

instance Alternative m => Alternative (Interspersed m) where
  empty =
    Interspersed (const (empty, False))
  (<|>) (Interspersed impl1) (Interspersed impl2) =
    Interspersed impl3
    where
      impl3 separator =
        case impl1 separator of
          (m1, nonEmpty1) ->
            case impl2 separator of
              (m2, nonEmpty2) ->
                (m1 <|> m2, nonEmpty1 || nonEmpty2)

runInterspersed :: Interspersed m a -> m () -> m a
runInterspersed =
  undefined

interspersed :: m a -> Interspersed m a
interspersed =
  undefined
