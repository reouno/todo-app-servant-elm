module Util.Time exposing (..)

import String exposing (fromInt, padLeft)
import Task
import Time exposing (..)

toString : Zone -> Posix -> String
toString zone time =
    let
        yy = padLeft 4 '0' <| fromInt <| (toYear zone time)
        mm = padLeft 2 '0' <| fromInt <| toIntMonth <| (toMonth zone time)
        dd = padLeft 2 '0' <| fromInt <| (toDay zone time)
        h  = padLeft 2 '0' <| fromInt <| (toHour zone time)
        m  = padLeft 2 '0' <| fromInt <| (toMinute zone time)
        s  = padLeft 2 '0' <| fromInt <| (toSecond zone time)
    in
        yy ++ "-" ++ mm ++ "-" ++ dd ++ "T" ++ h ++ ":" ++ m ++ ":" ++ s ++ "Z"

toStringUTC : Posix -> String
toStringUTC time = toString utc time

toIntMonth : Month -> Int
toIntMonth month =
    case month of
        Jan -> 1
        Feb -> 2
        Mar -> 3
        Apr -> 4
        May -> 5
        Jun -> 6
        Jul -> 7
        Aug -> 8
        Sep -> 9
        Oct -> 10
        Nov -> 11
        Dec -> 12

msgWithTime : (Time.Posix -> msg) -> Task.Task Never Time.Posix -> Cmd msg
msgWithTime msg time =
    Task.perform msg time
