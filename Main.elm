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

type Msg = AddKey String | Generate

update : Msg -> Model -> (Model, Cmd Msg)
update msg model = 
    case msg of
        Generate ->
            -- the "test string here is nessicary to satify the type checker. I
            -- would like to factor out.
            model ! [Key.generate <| AddKey "test"]
        AddKey key ->
            (key :: model) ! []

preCode attrs content =
    pre [] [ code [] [ text content ] ]

view model = 
    div [] 
        [ button [onClick Generate] [text "Generate"]
        , preCode [] <| String.join "\n" model
        ]
