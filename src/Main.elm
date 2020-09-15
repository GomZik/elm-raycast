module Main exposing (main)

import Browser
import Browser.Events exposing ( onAnimationFrameDelta, onKeyDown, onKeyUp )
import Html exposing (..)
import Html.Attributes exposing ( style )
import Json.Decode as JD exposing ( Decoder )

import RayCast exposing ( RayCast )
import Map exposing ( Tile )
import Angle

simpleMap : List String
simpleMap =
  [ "#########"
  , "#.......#"
  , "#.......#"
  , "#.......#"
  , "#.......#"
  , "#.......#"
  , "#.......#"
  , "#.......#"
  , "#########"
  ]

type Msg
  = NoOp
  | RAF Float
  | KeyDown Key
  | KeyUp Key

type Key = Up | Down | Left | Right

keyDecoder : Decoder Key
keyDecoder =
  JD.field "key" JD.string
    |> JD.andThen (\c ->
      case c of
        "w" -> JD.succeed Up
        "s" -> JD.succeed Down
        "a" -> JD.succeed Left
        "d" -> JD.succeed Right
        _ -> JD.fail "unrecogniezed key"
    )


type alias Model = Result String
  { rc : RayCast Tile Msg
  , up : Bool
  , down : Bool
  , left : Bool
  , right : Bool
  }

main : Program () Model Msg
main =
  Browser.document
    { init = init
    , view = viewDoc
    , update = update
    , subscriptions = subscriptions
    }


renderTile : Tile -> RayCast.Shape Msg
renderTile t =
  case t of
    Map.Free -> RayCast.none
    Map.Wall -> RayCast.solid


init : () -> ( Model, Cmd Msg )
init _ =
  simpleMap
    |> Map.parse
    |> Result.mapError Map.errorToString
    |> Result.map (\m ->
      { rc = m
          |> RayCast.init
          |> RayCast.setCamerAngle ( Angle.fromDegrees 90 )
          |> RayCast.setRenderer renderTile
      , up = False
      , down = False
      , left = False
      , right = False
      }
    )
    |> (\x -> ( x, Cmd.none ))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case model of
    Err _ -> ( model, Cmd.none )
    Ok m ->
      case msg of
        NoOp -> ( Ok m, Cmd.none )
        KeyDown Up -> ( Ok { m | up = True }, Cmd.none )
        KeyUp Up -> ( Ok { m | up = False }, Cmd.none )
        KeyDown Down -> ( Ok { m | down = True }, Cmd.none )
        KeyUp Down -> ( Ok { m | down = False }, Cmd.none )
        KeyDown Left -> ( Ok { m | left = True }, Cmd.none )
        KeyUp Left -> ( Ok { m | left = False }, Cmd.none )
        KeyDown Right -> ( Ok { m | right = True }, Cmd.none )
        KeyUp Right -> ( Ok { m | right = False }, Cmd.none )
        RAF dt ->
          let
            rc = m.rc
              |> RayCast.updateCamera (\( ( x, y ), a ) ->
                let
                  speed = case ( m.up, m.down ) of
                    ( True, False ) -> -64 * dt / 1000
                    ( False, True ) -> 64 * dt / 1000
                    _ -> 0
                  dx = speed * cos ( Angle.toRadians a )
                  dy = speed * sin ( Angle.toRadians a )
                  angleSpeed = case ( m.left, m.right ) of
                    ( True, False ) -> -1 * ( turns 1/2 ) * dt / 1000
                    ( False, True ) -> ( turns 1/2 ) * dt / 1000
                    _ -> 0
                in
                  Debug.log "NewCoords" <|
                    (
                      ( round <| toFloat x + dx, round <| toFloat y + dy )
                    , a
                      |> Angle.toRadians
                      |> (+) angleSpeed
                      |> Angle.fromRadians
                    )
              )
          in
            ( Ok { m | rc = rc }, Cmd.none )



subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch
   [ onKeyDown ( JD.map KeyDown keyDecoder )
   , onKeyUp ( JD.map KeyUp keyDecoder )
   , onAnimationFrameDelta RAF
   ]

viewDoc : Model -> Browser.Document Msg
viewDoc model =
  { title = "RayCast example"
  , body = [view model]
  }

view : Model -> Html Msg
view modelR =
  case modelR of
    Err err -> h1 [] [ text err ]
    Ok model ->
      div []
        [ h1 [] [ text "RayCast example" ]
        , RayCast.viewport
          [ style "width" "640px"
          , style "height" "480px"
          , style "display" "block"
          , style "background-color" "black"
          ] model.rc
        ]