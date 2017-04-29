module TryIndex

import Data.Vect

tryIndex : Integer -> Vect n a -> Maybe a
tryIndex {n} x xs =
  case integerToFin x n of
        Nothing => Nothing
        (Just i) => Just (Data.Vect.index i xs)
