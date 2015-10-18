module Unsoundscapes (inputs, init, update, view, Model) where

import Effects exposing (Effects)
import Html exposing (img, div, text, Html)
import Html.Attributes exposing (style, src)
import Html.Events exposing (onClick)
import Keyboard
import Mouse
import Signal exposing ((<~), (~), sampleOn)
import String
import Time exposing (fps)
import Window
import History
import Task


-- MODEL


type alias Circle =
  { x : Int
  , y : Int
  , r : Int
  }


moveCircle : Circle -> Int -> Int -> Circle
moveCircle circle x y =
  { circle | x <- x, y <- y }


resizeCircle : Circle -> Int -> Circle
resizeCircle circle dr =
  { circle | r <- circle.r + dr |> max 10 |> min 60 }


listToTuple3 : List Int -> (Int, Int, Int)
listToTuple3 l =
  let
    el1 = l |> List.head
    el2 = l |> List.drop 1 |> List.head
    el3 = l |> List.drop 2 |> List.head
  in
    ( Maybe.withDefault 0 el1
    , Maybe.withDefault 0 el2
    , Maybe.withDefault 0 el3
    )


tuple3ToList : (Int, Int, Int) -> List Int
tuple3ToList (a, b, c) =
  [a, b, c]


coordsToCircle : (Int, Int, Int) -> Circle
coordsToCircle (x, y, r) =
  Circle (x + r * 5) (y + r * 5) (r * 5)


circleToCoords : Circle -> (Int, Int, Int)
circleToCoords circle =
  (circle.x - circle.r, circle.y - circle.r, circle.r // 5)


coordsToCircles : List Int -> List Circle
coordsToCircles list =
  if List.length list >= 3
  then (
    List.take 3 list
      |> listToTuple3
      |> coordsToCircle
  ) :: coordsToCircles (List.drop 3 list)
  else []


circlesFromHash : String -> List Circle
circlesFromHash hash =
  String.dropLeft 1 hash
    |> String.split ","
    |> List.map (String.toInt >> Result.toMaybe >> Maybe.withDefault 0)
    |> coordsToCircles


hashFromCircles : List Circle -> String
hashFromCircles circles =
  circles
    |> List.map (circleToCoords >> tuple3ToList)
    |> List.concat
    |> List.map toString
    |> List.intersperse ","
    |> (::) "#"
    |> List.foldr (++) ""


type alias Model =
  { currentCircle : Circle
  , circles : List Circle
  }


init : Int -> (Model, Effects Action)
init r =
  ( Model (Circle 0 0 r) []
  , Effects.none
  )


-- UPDATE


type Action
  = Remove Circle
  | Add Circle
  | MoveCurrent (Int, Int)
  | SizeUp
  | SizeDown
  | LoadCircles String
  | Noop


withHashChange : Model -> (Model, Effects Action)
withHashChange model =
  ( model
  , History.setPath (hashFromCircles model.circles)
      |> Task.map (always Noop)
      |> Effects.task
  )


update : Action -> Model -> (Model, Effects Action)
update action model =
  case action of
    LoadCircles hash ->
      (,) { model | circles <- circlesFromHash hash} Effects.none
    MoveCurrent (x, y) ->
      (,) { model | currentCircle <- moveCircle model.currentCircle x y } Effects.none
    Add circle ->
      withHashChange { model | circles <- model.currentCircle :: model.circles }
    Remove circle ->
      withHashChange { model | circles <- List.filter ((/=) circle) model.circles }
    SizeUp ->
      (,) { model | currentCircle <- resizeCircle model.currentCircle 5 } Effects.none
    SizeDown ->
      (,) { model | currentCircle <- resizeCircle model.currentCircle -5 } Effects.none
    Noop ->
      (model, Effects.none)


keyDown : Int -> Signal Bool
keyDown c =
  Signal.filter identity False (Signal.sampleOn (fps 10) (Keyboard.isDown c))


inputs : List (Signal Action)
inputs =
  let
    offsetBy (x1, y1) (x2, y2) = (x2 - x1, y2 - y1)
    imageOffset = offsetBy (780, 680) <~ Window.dimensions
  in
    [ LoadCircles <~ History.hash -- has to be the first element for initial state
    , MoveCurrent <~ (offsetBy <~ imageOffset ~ Mouse.position)
    , always SizeUp <~ keyDown 221
    , always SizeDown <~ keyDown 219
    ]


-- VIEW


toPx : Int -> String
toPx val =
  toString val ++ "px"


renderCircle : Signal.Address Action -> Bool -> Circle -> Html
renderCircle address isCurrent circle =
  div [
    onClick address (if isCurrent then Add circle else Remove circle)
  , style
    [ ("background-image", "url(../images/hair.jpg)")
    , ("background-repeat", "no-repeat")
    , ("border-radius", "50%")
    , ("position", "absolute")
    , ("width", toPx (circle.r * 2))
    , ("height", toPx (circle.r * 2))
    , ("left", toPx (circle.x - circle.r))
    , ("top", toPx (circle.y - circle.r))
    , ("background-position", toPx (circle.r - circle.x) ++ " " ++ toPx (circle.r - circle.y))
    ]
  ]
  []


view : Signal.Address Action -> Model -> Html
view address model =
  div
  [ style
    [ ("background-image", "url(../images/bald.jpg)")
    , ("bottom", "0")
    , ("height", toPx 680)
    , ("overflow", "hidden")
    , ("position", "absolute")
    , ("right", "0")
    , ("width", toPx 780)
    ]
  ]
  ( renderCircle address True model.currentCircle ::
    List.map (renderCircle address False) model.circles
  )
