module Main exposing (main)

import Html exposing (..)
import Html.Events exposing (onClick)
import Key

main : Program Never Model Msg
main = Html.program
    { init = [] ! 
        []
    , update = update
    , view = view
    , subscriptions = \_ -> Sub.none
    }

type alias Model = List String

type Msg 
    = AddKey String
    | Generate
    | GenerateMultiple

update : Msg -> Model -> (Model, Cmd Msg)
update msg model = 
    case msg of
        Generate ->
            model ! [Key.generate AddKey]
        GenerateMultiple ->
            ( model
            , Cmd.batch 
                [ Key.generate AddKey
                , Key.generate AddKey
                , Key.generate AddKey
                , Key.generate AddKey
                , Key.generate AddKey
                , Key.generate AddKey
                , Key.generate AddKey
                ]
            )
        AddKey key ->
            (key :: model) ! []

preCode attrs content =
    pre [] [ code [] [ text content ] ]

view model = 
    div [] 
        [ button [onClick Generate] [text "Generate"]
        , button [onClick GenerateMultiple] [text "Generate Multiple"]
        , preCode [] <| String.join "\n" model
        ]
