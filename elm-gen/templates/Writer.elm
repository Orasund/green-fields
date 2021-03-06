module {{moduleBase}}.{{template}}.{{moduleName}} exposing (..)

{-| Module generated by [Elm-Gen](https://orasund.github.io/elm-gen).

This module contains the {{moduleName}} {{template}}.

    type alias {{moduleName}} state{{#if output.isPolymorphic}} out{{/if}} =
        ( state, {{output.type}}{{#if output.isPolymorphic}} out{{/if}} )

This module implements the writer monad.

That's just a fancy way of saying that it using `state` for computation and stores the result in `{{output.type}}`.


# Basics

@docs {{moduleName}}, start, andThen, stop


# Pause and Continue

@docs pause, continue


# Modifications

@docs map, mapOutput

-}

{{#imports}}{{.}}{{/imports}}

-- This is a generated file. DO NOT CHANGE ANYTHING IN HERE.

-------------------------------------------------------------------------------
-- BASICS
-------------------------------------------------------------------------------


{-| {{moduleName}} type 
-}
type alias {{moduleName}} state{{#if output.isPolymorphic}} out{{/if}} =
    ( state, {{output.type}}{{#if output.isPolymorphic}} out{{/if}} )


{-| Start the writing process.

    start : state -> {{moduleName}} state{{#if output.isPolymorphic}} out{{/if}}
    start a =
        ( a, {{output.empty}} )

Use `andThen` for computation steps.
Use `stop` to end the process and get the output.

If you already have a {{output.type}} that you want to continue on, use `continue` instead.

-}
start : state -> {{moduleName}} state{{#if output.isPolymorphic}} out{{/if}}
start a =
    ( a, {{output.empty}} )


{-| Apply a computation to the writer.

    andThen : (state1 -> {{moduleName}} state2{{#if output.isPolymorphic}} out{{/if}}) -> {{moduleName}} state1{{#if output.isPolymorphic}} out{{/if}} -> {{moduleName}} state2{{#if output.isPolymorphic}} out{{/if}}
    andThen fun ( a, out ) =
        let
            ( newA, newOut ) =
                fun a
        in
        ( newA
        , out |> {{output.append}} newOut
        )

Note that {{moduleName}} is just a tuple, where the output is stored in the second argument.

-}
andThen : (state1 -> {{moduleName}} state2{{#if output.isPolymorphic}} out{{/if}}) -> {{moduleName}} state1{{#if output.isPolymorphic}} out{{/if}} -> {{moduleName}} state2{{#if output.isPolymorphic}} out{{/if}}
andThen fun ( a, out ) =
    let
        ( newA, newOut ) =
            fun a
    in
    ( newA
    , out |> {{output.append}} newOut
    )


{-| Stop the writer, turn the current state into {{output.type}} and return the output.

    stop : ( state -> {{output.type}}{{#if output.isPolymorphic}} out{{/if}} ) -> {{moduleName}} state{{#if output.isPolymorphic}} out{{/if}} -> {{output.type}}{{#if output.isPolymorphic}} out{{/if}}
    stop fun ( a, out) =
        out |> {{output.append}} (fun a)

If you want to ignore the current state and just get the current output, use `pause` instead.
-}
stop : ( state -> {{output.type}}{{#if output.isPolymorphic}} out{{/if}} ) -> {{moduleName}} state{{#if output.isPolymorphic}} out{{/if}} -> {{output.type}}{{#if output.isPolymorphic}} out{{/if}}
stop fun ( a, out ) =
    out |> {{output.append}} (fun a)



-------------------------------------------------------------------------------
-- PAUSE AND CONTINUE
-------------------------------------------------------------------------------


{-| Get the current output.

    pause : {{moduleName}} state{{#if output.isPolymorphic}} out{{/if}} -> {{output.type}}{{#if output.isPolymorphic}} out{{/if}}
    pause =
        Tuple.second

-}
pause : {{moduleName}} state{{#if output.isPolymorphic}} out{{/if}} -> {{output.type}}{{#if output.isPolymorphic}} out{{/if}}
pause =
    Tuple.second

{-| Continue the computation with a {{output.type}}.

    continue : {{output.type}}{{#if output.isPolymorphic}} out{{/if}} -> state -> {{moduleName}} state{{#if output.isPolymorphic}} out{{/if}}
    continue out a =
        ( a, out )

If you have already a {{moduleName}} that you want to continue using, use `mapOutput` instead.

-}
continue : {{output.type}}{{#if output.isPolymorphic}} out{{/if}} -> state -> {{moduleName}} state{{#if output.isPolymorphic}} out{{/if}}
continue out a =
    ( a, out )

-------------------------------------------------------------------------------
-- MODIFICATIONS
-------------------------------------------------------------------------------

{-| Map the state of a {{moduleName}}.

    map : ( state1 -> state2 ) -> {{moduleName}} state1{{#if output.isPolymorphic}} out{{/if}} -> {{moduleName}} state2{{#if output.isPolymorphic}} out{{/if}}
    map fun =
        Tuple.mapFirst fun

-}
map : ( state1 -> state2 ) -> {{moduleName}} state1{{#if output.isPolymorphic}} out{{/if}} -> {{moduleName}} state2{{#if output.isPolymorphic}} out{{/if}}
map fun =
    Tuple.mapFirst fun

{-| Map the output of a {{moduleName}}

    mapOutput : ( {{output.type}} {{#if output.isPolymorphic}}out1{{/if}} -> {{output.type}} {{#if output.isPolymorphic}}out2{{/if}} ) -> {{moduleName}} state {{#if output.isPolymorphic}}out1{{/if}} -> {{moduleName}} state {{#if output.isPolymorphic}}out2{{/if}}
    mapOutput fun =
        Tuple.mapSecond fun

-}
mapOutput : ( {{output.type}} {{#if output.isPolymorphic}}out1{{/if}} -> {{output.type}} {{#if output.isPolymorphic}}out2{{/if}} ) -> {{moduleName}} state {{#if output.isPolymorphic}}out1{{/if}} -> {{moduleName}} state {{#if output.isPolymorphic}}out2{{/if}}
mapOutput fun =
    Tuple.mapSecond fun

-- Generated with [Elm-Gen](https://orasund.github.io/elm-gen) Version {{version}}