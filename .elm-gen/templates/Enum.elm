module {{moduleBase}}.{{template}}.{{typeName}} exposing (..)

{-| This is a generated file. DO NOT CHANGE ANYTHING.

# Basics

@docs {{typeName}},asList

# Converters

@docs toInt,fromInt,toString,fromString

-}

type {{typeName}}
{{#arguments}}
{{#unless @index}}   = {{.}}
{{else}}   | {{.}}
{{/unless}}
{{/arguments}}

asList : List {{typeName}}
asList =
{{#arguments}}{{#unless @index}}  [ {{.}}{{else}}
  , {{.}}{{/unless}}{{/arguments}}
  ]

toInt : {{typeName}} -> Int
toInt arg =
  case arg of
{{#arguments}}
    {{.}} -> {{@index}}
{{/arguments}}

fromInt : Int -> Maybe {{typeName}}
fromInt int =
  case int of
{{#arguments}}
    {{@index}} -> Just {{.}} 
{{/arguments}}
    _ -> Nothing
    
toString : {{typeName}} -> String
toString arg =
  case arg of
{{#arguments}}
    {{.}} -> "{{.}}"
{{/arguments}}

fromString : String -> Maybe {{typeName}}
fromString arg =
  case arg of
{{#arguments}}
    "{{.}}" -> Just {{.}} 
{{/arguments}}
    _ -> Nothing