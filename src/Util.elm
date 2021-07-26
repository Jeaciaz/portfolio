module Util exposing (..)


mapAcc : (a -> acc -> ( b, acc )) -> acc -> List a -> ( List b, acc )
mapAcc f acc0 list =
    case list of
        [] ->
            ( [], acc0 )

        x :: xs ->
            let
                ( y, acc1 ) =
                    f x acc0

                ( ys, acc2 ) =
                    mapAcc f acc1 xs
            in
            ( y :: ys, acc2 )
