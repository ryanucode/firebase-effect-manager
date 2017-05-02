effect module Key where { command = MgrCmd } exposing (generate)

import Task exposing (Task)
import Random
import Random.String as Random
import Time

{-| 
    **WARNING** Do not call this multiple times with the same seed! It will
    generate identical keys. Please use `generateKeys` instead to aquire
    multiple unique ordered keys.

    For those of you thinking you can initialize the seet from the time we pass
    in **DO NOT DO IT**.  If you examine the
    [original firebase code](https://gist.github.com/mikelehen/3596a30bd69384624c11)
    you will notice that they have a duplicate time check. We can safely
    avoid that check (and since elm's Random implementation is deterministic
    we have to!) by using as seed initialized at a time independent of the
    time the key is generated at.
--}
generateKey : Random.Seed -> Task Never (String, Random.Seed)
generateKey seed =
    let
        (randString, newSeed) = Random.step (Random.string 12 charGenerator) seed
        pred time =
            (timeString time ++ randString, newSeed)
    in
        Task.map pred Time.now

pool : String
pool =
    "-0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ_abcdefghijklmnopqrstuvwxyz"

charInPool : Int -> Char
charInPool index =
    String.slice index (index + 1) pool
        |> String.uncons
        |> Maybe.map Tuple.first
        |> Maybe.withDefault '*'

charGenerator : Random.Generator Char
charGenerator =
    Random.map charInPool <| Random.int 0 63

timeString : Float -> String
timeString time =
    let
        timeBase64 power =
            floor (time / (64 ^ toFloat power)) % 64
    in
        List.range 0 7
            |> List.map (timeBase64 >> charInPool)
            |> List.reverse
            |> String.fromList


-- EFFECT MANAGER

type MgrCmd msg = Generate (String -> msg)

type alias State =
    Random.Seed

generate : (String -> msg) -> Cmd msg
generate =
    command << Generate

cmdMap : (a -> b) -> MgrCmd a -> MgrCmd b
cmdMap func (Generate a) =
    Generate <| func << a

init : Task Never State
init =
    Task.map (Debug.log "initialized" << Random.initialSeed << round) Time.now

onEffects : Platform.Router msg Never -> List (MgrCmd msg) -> State -> Task Never State
onEffects router commands seed =
    case commands of
        [] ->
            Task.succeed seed
        Generate tagger :: rest ->
            let
                route (key, newSeed) =
                    Platform.sendToApp router (tagger key)
                        |> Task.andThen (\_ -> onEffects router rest (Debug.log "new seed persisted" newSeed))
            in
                Task.andThen route <| generateKey seed

onSelfMsg : Platform.Router msg Never -> Never -> State -> Task Never State
onSelfMsg _ _ seed =
    Debug.log "onSelfMsg" <| Task.succeed seed
