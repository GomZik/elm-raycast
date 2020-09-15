module RayCast exposing (..)

import Html exposing ( Html )
import Svg exposing (..)
import Svg.Attributes exposing (..)

import Array exposing ( Array )

import Angle exposing ( Angle )
import Mem
import Mem


type alias Map map = Array ( Array map )

type alias Renderer map msg = ( map -> Shape msg )

type alias HeightGetter map = ( map -> Int )

type Mem a = Mem ( Mem.Mem Float Float )

type Sin = Sin

type Cos = Cos

type Tn = Tn

type alias Internals map msg =
  { fov : Angle
  , map : Map map
  , renderer : Renderer map msg
  , width : Int
  , height : Int
  , tileSize : Int
  , tileHeight : HeightGetter map
  , cameraWorldPosition : ( Int, Int )
  , cameraAngle : Angle
  , tanMem : Mem Tn
  , sinMem : Mem Sin
  , cosMem : Mem Cos
  }

type RayCast map msg = RayCast ( Internals map msg )

type Shape msg
  = None
  | Solid


none : Shape msg
none = None

solid : Shape msg
solid = Solid


init : Map map -> RayCast map msg
init map =
  RayCast
    { fov = Angle.fromDegrees 0
    , map = map
    , renderer = always none
    , width = 320
    , height = 200
    , tileSize = 64
    , tileHeight = always 64
    , cameraAngle = Angle.fromDegrees 135
    , cameraWorldPosition = ( 64 * 4, 80 )
    , tanMem = List.range 0 360
      |> List.map ( Angle.fromDegrees >> Angle.toRadians )
      |> (\x -> Mem.fill x ( Mem.empty tan ) )
      |> Mem
    , sinMem = List.range 0 360
      |> List.map ( Angle.fromDegrees >> Angle.toRadians )
      |> (\x -> Mem.fill x ( Mem.empty sin ) )
      |> Mem
    , cosMem = List.range 0 360
      |> List.map ( Angle.fromDegrees >> Angle.toRadians )
      |> (\x -> Mem.fill x ( Mem.empty cos ) )
      |> Mem
    }

setFov : Angle -> RayCast map msg -> RayCast map msg
setFov fov ( RayCast d ) =
  RayCast { d | fov = fov }

setDimensions : Int -> Int -> RayCast map msg -> RayCast map msg
setDimensions w h ( RayCast d ) =
  RayCast { d | width = w, height = h }


setTileSize : Int -> RayCast map msg -> RayCast map msg
setTileSize s ( RayCast d ) =
  RayCast { d | tileSize = s }


setTileHeight : ( map -> Int ) -> RayCast map msg -> RayCast map msg
setTileHeight fn ( RayCast d ) =
  RayCast { d | tileHeight = fn }


setCameraPosition : Int -> Int -> RayCast map msg -> RayCast map msg
setCameraPosition x y ( RayCast d ) =
  RayCast { d | cameraWorldPosition = ( x, y ) }


setCamerAngle : Angle -> RayCast map msg -> RayCast map msg
setCamerAngle a ( RayCast d ) =
  RayCast { d | cameraAngle = a }


setRenderer : Renderer map msg -> RayCast map msg -> RayCast map msg
setRenderer fn ( RayCast d ) =
  RayCast { d | renderer = fn }


updateMap : ( Map map -> Map map ) -> RayCast map msg -> RayCast map msg
updateMap fn ( RayCast d ) =
  RayCast { d | map = fn d.map }


updateCameraPosition : ( ( Int, Int ) -> ( Int, Int ) ) -> RayCast map msg -> RayCast map msg
updateCameraPosition fn ( RayCast d ) =
  RayCast { d | cameraWorldPosition = fn d.cameraWorldPosition }


updateCameraAngle : ( Angle -> Angle ) -> RayCast map msg -> RayCast map msg
updateCameraAngle fn ( RayCast d ) =
  RayCast { d | cameraAngle = fn d.cameraAngle }


updateCamera : ( ( ( Int, Int ), Angle ) -> ( ( Int, Int ), Angle ) ) -> RayCast map msg -> RayCast map msg
updateCamera fn ( RayCast d ) =
  let
    ( pos, an ) = fn ( d.cameraWorldPosition, d.cameraAngle )
  in
    RayCast { d | cameraWorldPosition = pos, cameraAngle = an }

getDistance : Map map -> Renderer map msg -> HeightGetter map -> Mem Sin -> Mem Cos -> Int -> Int -> Int -> Int -> Int -> Int -> ( Int, Int )
getDistance map renderer tileHeight sinMem cosMem ax ay px py dx dy =
      map
        |> Array.get ( ay // 64 )
        |> Maybe.withDefault Array.empty
        |> Array.get ( ax // 64 )
        |> Maybe.map (\m -> ( tileHeight m, renderer m))
        |> Maybe.withDefault ( 0, Solid )
        |> (\(h, s) ->
            case s of
              None -> getDistance map renderer tileHeight sinMem cosMem ( ax + dx ) ( ay + dy ) px py dx dy
              Solid ->
                ( h, floor ( sqrt ( toFloat ( ( px - ax ) ^ 2 + ( py - ay ) ^ 2 ) ) ) )
          )

traceHorizontal : RayCast map msg -> Angle -> ( Int, Int )
traceHorizontal ( RayCast d ) a =
  let
    ( px, py ) = d.cameraWorldPosition
    cmp = Angle.compare a ( Angle.fromDegrees 180 )
    ay = case cmp of
      LT -> py // d.tileSize * d.tileSize - 1
      _ -> py // d.tileSize * d.tileSize + d.tileSize
    
    ( Mem tnMem ) = d.tanMem
    tn = Mem.get ( Angle.toRadians a ) tnMem
      |> Tuple.first
    ax = px + floor ( toFloat ( py - ay ) / tn )
    ya = case cmp of
      LT -> -d.tileSize
      _ -> d.tileSize
    xa = floor ( toFloat d.tileSize / tn )

  in
    getDistance d.map d.renderer d.tileHeight d.sinMem d.cosMem ax ay px py xa ya

traceVertical : RayCast map msg -> Angle -> ( Int, Int )
traceVertical ( RayCast d ) a =
  let
    ( px, py ) = d.cameraWorldPosition
    ( cmp1, cmp2 ) = ( Angle.compare ( Angle.fromDegrees 90 ) a, Angle.compare ( Angle.fromDegrees 270 ) a )

    isLeft = case ( cmp1, cmp2 ) of
      ( GT, LT ) -> False
      _ -> True

    ax = if isLeft
      then px // d.tileSize * d.tileSize - 1
      else px // d.tileSize * d.tileSize + d.tileSize

    ( Mem tanMem ) = d.tanMem

    tn = Mem.get ( Angle.toRadians a ) tanMem
      |> Tuple.first

    ay = py + floor ( toFloat ( px - ax ) * tn )
    xa = if isLeft then -d.tileSize else d.tileSize
    ya = floor ( toFloat d.tileSize * tn )
  in
    getDistance d.map d.renderer d.tileHeight d.sinMem d.cosMem ax ay px py xa ya


castColumn : Int -> RayCast map msg -> Html msg
castColumn n ( ( RayCast d ) as rc ) =
  let
    startAngle = d.cameraAngle
      |> Angle.toRadians
      |> (\a -> a - Angle.toRadians d.fov / 2 )
      |> Angle.fromRadians

    currentAngle = startAngle
      |> Angle.toRadians
      |> (\a -> a + toFloat n * Angle.toRadians d.fov / toFloat d.width )
      |> Angle.fromRadians
    horizontalDist = traceHorizontal rc currentAngle
    verticalDist = traceVertical rc currentAngle
    ( height, distance ) = if Tuple.second horizontalDist > Tuple.second verticalDist then verticalDist
                           else horizontalDist

    projectionHeight = toFloat height / toFloat distance * 277
    top = toFloat d.height / 2  - projectionHeight / 2
  in
    line
      [ x1 <| String.fromInt n
      , x2 <| String.fromInt n
      , y1 <| String.fromFloat top
      , y2 <| String.fromFloat ( top + projectionHeight )
      , stroke "blue"
      ] []

cast : RayCast map msg -> List ( Html msg )
cast ( ( RayCast d ) as rs ) =
  List.range 0 d.width
    |> List.map (\x -> castColumn x rs)

viewport : List ( Attribute msg ) -> RayCast map msg -> Html msg
viewport attrs ( ( RayCast d ) as rc ) =
  svg
    ( ( viewBox <| "0 0 " ++ String.fromInt d.width ++ " " ++ String.fromInt d.height )
      :: attrs
    ) <| cast rc
