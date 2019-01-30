module Util.String exposing (..)

joinPath : String -> String -> String
joinPath s1 s2 =
    let
        s1_ = if String.endsWith "/" s1
            then String.dropRight 1 s1
            else s1
        s2_ = if String.startsWith "/" s2
            then String.dropLeft 1 s2
            else s2
    in
        s1 ++ "/" ++ s2
