module Styles.BulmaModifiers exposing (..)

import Bulma.Columns exposing (..)
import Bulma.Components exposing (..)
import Bulma.Elements exposing (..)
import Bulma.Form exposing (..)
import Bulma.Layout exposing (..)
import Bulma.Modifiers exposing (..)

headerModifiers : HeroModifiers
headerModifiers =
    { bold = False
    , size = Small
    , color = Light
    }

mainTabsModifiers : TabsModifiers
mainTabsModifiers =
    { style = Minimal
    , alignment = Centered
    , size = Small
    }

todoColsModifiers : ColumnsModifiers
todoColsModifiers =
    { multiline = False
    , gap = Gap0
    , display = MobileAndBeyond
    , centered = False
    }

checkColModifiers : ColumnModifiers
checkColModifiers =
    { offset = Auto
    , widths = constWidth (Just Width1)
    }

mainColModifiers : ColumnModifiers
mainColModifiers =
    { offset = Auto
    , widths = constWidth (Just Auto)
    }

delColModifiers : ColumnModifiers
delColModifiers =
    { offset = Auto
    , widths = constWidth (Just Width2)
    }

delBtnModifiers : ControlInputModifiers msg
delBtnModifiers =
    { size     = Small
    , state    = Blur
    , color    = Danger
    , expanded = True
    , rounded  = False
    , readonly = False
    , disabled = False
    , iconLeft = Nothing
    , iconRight = Nothing
    }

constWidth : Maybe Width -> Devices (Maybe Width)
constWidth mw =
    { mobile = mw
    , tablet = mw
    , desktop = mw
    , widescreen = mw
    , fullHD = mw
    }
