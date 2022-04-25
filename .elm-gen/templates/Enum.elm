module {{moduleBase}}.{{template}}.{{typeName}} exposing (..)

{-| This is a generated file. DO NOT CHANGE ANYTHING.


# Basics

@docs {{typeName}}, asList{{#constructors}}, {{decapitalize .}}{{/constructors}}


# Converters

@docs toInt, fromInt, toString, fromString

-}

-------------------------------------------------------------------------------
-- BASICS
-------------------------------------------------------------------------------

type {{typeName}}
{{#constructors}}
{{#if @first}}   = {{.}}
{{else}}   | {{.}}
{{/if}}
{{/constructors}}

asList : List {{typeName}}
asList =
{{#constructors}}{{#if @first}}    [ {{.}}{{else}}
    , {{.}}{{/if}}{{/constructors}}
    ]

{{#constructors}}
{{decapitalize .}} : {{../typeName}}
{{decapitalize .}} =
    {{.}}

{{/constructors}}

-------------------------------------------------------------------------------
-- CONVERTERS
-------------------------------------------------------------------------------

toInt : {{typeName}} -> Int
toInt arg =
    case arg of
{{#constructors}}
        {{.}} -> {{@index}}
{{/constructors}}

fromInt : Int -> Maybe {{typeName}}
fromInt int =
    case int of
{{#constructors}}
        {{@index}} -> Just {{.}} 
{{/constructors}}
        _ -> Nothing
    
toString : {{typeName}} -> String
toString arg =
    case arg of
{{#constructors}}
        {{.}} -> "{{.}}"
{{/constructors}}

fromString : String -> Maybe {{typeName}}
fromString arg =
    case arg of
{{#constructors}}
        "{{.}}" -> Just {{.}} 
{{/constructors}}
        _ -> Nothing