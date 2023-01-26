module List.Extra exposing (any, indexedMap, reverse, snoc)

-- List Functions


snoc : List a -> a -> List a
snoc list a =
    case list of
        [] ->
            [ a ]

        first :: remaininglist ->
            first :: snoc remaininglist a


reverse : List a -> List a
reverse list =
    case list of
        [] ->
            []

        first :: remaininglist ->
            snoc (reverse remaininglist) first


any : (a -> Bool) -> List a -> Bool
any pred list =
    case list of
        [] ->
            False

        first :: remaininglist ->
            pred first || any pred remaininglist


indexedMap : (Int -> a -> b) -> List a -> List b
indexedMap func list =
    let
        indexedMapHelper : List a -> Int -> List b
        indexedMapHelper li index =
            case li of
                [] ->
                    []

                first :: remaininglist ->
                    func index first :: indexedMapHelper remaininglist (index + 1)
    in
    indexedMapHelper list 0
