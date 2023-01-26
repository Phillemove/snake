module List.NonEmpty exposing (NonEmpty(..), head, last, removeLast, snoc, toList)

import List.Extra



-- Declaration and functions for NonEmpty List


type NonEmpty a
    = Head a (List a)


head : NonEmpty a -> a
head (Head a _) =
    a


last : NonEmpty a -> a
last nonempty =
    case nonempty of
        Head a [] ->
            a

        Head _ (first :: remaininglist) ->
            last (Head first remaininglist)


removeLast : NonEmpty a -> List a
removeLast nonempty =
    case nonempty of
        Head _ [] ->
            []

        Head a (first :: remaininglist) ->
            a :: removeLast (Head first remaininglist)


snoc : NonEmpty a -> a -> NonEmpty a
snoc (Head ha list) a =
    Head ha (List.Extra.snoc list a)


toList : NonEmpty a -> List a
toList (Head elem list) =
    elem :: list
