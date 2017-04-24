effect module Key where { command = MgrCmd } exposing (..)

import Task exposing (Task)
import Random
import Random.String as Random
import Time


-- firebase key generation
generateKeys : Random.Seed -> Int -> Task Never (List String, Random.Seed)
generateKeys seed count =
    let
        tail currentCount thisSeed keys =
            if currentCount <= 0 then
                Task.succeed (List.sort keys, thisSeed)
            else
                generateKey thisSeed
                    |> Task.andThen
                        (\(key, newSeed) -> tail (currentCount - 1) newSeed (key :: keys))
    in
        tail count seed []

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
|-}
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
        timeMod mod =
            floor (time / (64 ^ toFloat mod)) % 64
    in
        List.range 0 7
            |> List.map (timeMod >> charInPool)
            |> List.reverse
            |> String.fromList


-- EFFECT MANAGER

type MgrCmd msg = Generate msg

type alias State =
    Random.Seed

generate : msg -> Cmd msg
generate =
    command << Generate

cmdMap : (a -> b) -> MgrCmd a -> MgrCmd b
cmdMap func (Generate a) =
    Generate <| func a

init : Task Never State
init =
    Task.map (Random.initialSeed << round) Time.now

onEffects : Platform.Router msg Never -> List (MgrCmd msg) -> State -> Task Never State
onEffects router commands seed =
    case commands of
        [] ->
            Task.succeed seed
        Generate tagger :: rest ->
            let
                route (key, newSeed) =
                    Platform.sendToApp router tagger
                        |> Task.andThen (\_ -> onEffects router rest newSeed)
            in
                Task.andThen route <| generateKey seed

onSelfMsg : Platform.Router msg Never -> Never -> State -> Task Never State
onSelfMsg _ _ seed =
    Task.succeed seed
