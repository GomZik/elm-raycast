module Map exposing (..)

import Array exposing ( Array )
import Parser exposing ( Parser, (|=), (|.) )


type Tile = Free | Wall


wallParser : Parser ()
wallParser = Parser.symbol "#"


freeParser : Parser ()
freeParser = Parser.symbol "."


tileParser : Parser Tile
tileParser =
  Parser.oneOf
    [ Parser.map ( always Wall ) wallParser
    , Parser.map ( always Free ) freeParser
    ]


mapLineParser : Parser ( Array Tile )
mapLineParser =
  let
    helper revTiles =
      Parser.oneOf
        [ Parser.succeed (\tile -> Parser.Loop (tile :: revTiles))
          |= tileParser
        , Parser.succeed ()
          |> Parser.map (\_ -> Parser.Done ( List.reverse revTiles ))
        ]
  in
    Parser.loop [] helper
      |> Parser.map Array.fromList


type Error = ParseError ( List Parser.DeadEnd )

errorToString : Error -> String
errorToString ( ParseError e ) =
  Parser.deadEndsToString e


apply : List ( Result x a ) -> Result x ( List a )
apply lst =
  case lst of
    x :: xs ->
      case x of
        Err e -> Err e
        Ok a ->
          case apply xs of
            Ok r -> Ok ( a :: r )
            Err e -> Err e
    [] ->
      Ok []


parse : List String -> Result Error ( Array ( Array Tile ) )
parse strs =
  strs
    |>
    List.map (\itm ->
      Parser.run mapLineParser itm
        |> Result.mapError ParseError
    )
    |> apply
    |> Result.map Array.fromList
