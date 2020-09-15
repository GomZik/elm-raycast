module Angle exposing
  ( Angle, fromDegrees, toRadians, compare
  , fromRadians
  )


type Angle = Angle Float

fromDegrees : Int -> Angle
fromDegrees x =
  let
    xN = modBy 360 x
    realX = if xN < 0 then 360 + xN else xN
  in
    Angle <| degrees <| toFloat realX


toRadians : Angle -> Float
toRadians ( Angle d ) =
  d


fromRadians : Float -> Angle
fromRadians f =
  let
    fn = if f < 0 then 2*pi + f else f
    realF = if fn > 2 * pi then fn - toFloat ( round ( fn / 2 / pi ) ) * 2 * pi else fn
  in
  radians realF
    |> Angle

compare : Angle -> Angle -> Order
compare ( Angle d1 ) ( Angle d2 ) =
  Basics.compare d1 d2
