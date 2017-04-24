module Main exposing (main)

import Html exposing (..)
import Html.Events exposing (onClick)
import Key

main : Program Never Model Msg
main = Html.program
    { init = [] ! 
        []
        --[ Key.generate AddKey
        --, Key.generate AddKey
        --]
    , update = update
    , view = view
    , subscriptions = \_ -> Sub.none
    }

type alias Model = List String

type Msg = AddKey String

update : Msg -> Model -> (Model, Cmd Msg)
update (AddKey key) model = (key :: model) ! []

preCode attrs content =
    pre [] [ code [] [ text content ] ]

view model = 
    div [] 
        [ button [onClick <| AddKey "hello"] [text "hello"]
        , preCode [] <| String.join "\n" model
        ]
