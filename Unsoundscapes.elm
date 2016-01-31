module Unsoundscapes (inputs, init, update, view, Model) where

import Effects exposing (Effects)
import Html exposing (img, div, text, Html)
import Html.Attributes exposing (style, src, class)
import Html.Events exposing (onClick)
import Keyboard
import Mouse
import Signal
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
  { circle | x = x, y = y }


resizeCircle : Circle -> Int -> Circle
resizeCircle circle dr =
  { circle | r = circle.r + dr |> max 10 |> min 120 }


circlesFromHash : String -> List Circle
circlesFromHash hash =
  let
    coordsToCircles list =
      case list of
        x :: y :: r :: l ->
          Circle (x + r * 5) (y + r * 5) (r * 5) :: coordsToCircles l
        _ ->
          []
  in
    String.dropLeft 1 hash
      |> String.split ","
      |> List.map (String.toInt >> Result.toMaybe >> Maybe.withDefault 0)
      |> coordsToCircles


hashFromCircles : List Circle -> String
hashFromCircles circles =
  circles
    |> List.map (\{x, y, r} -> [x - r, y - r, r // 5])
    |> List.concat
    |> List.map toString
    |> List.intersperse ","
    |> (::) "#"
    |> List.foldr (++) ""


type alias Model =
  { currentCircle : Circle
  , circles : List Circle
  }


init : Int -> String -> (Model, Effects Action)
init r hash =
  ( Model (Circle 0 0 r) (circlesFromHash hash)
  , Effects.none
  )


-- UPDATE


type Action
  = Remove Circle
  | AddCurrent
  | MoveCurrent (Int, Int)
  | ResizeCurrent Int
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
      (,) { model | circles = circlesFromHash hash} Effects.none
    MoveCurrent (x, y) ->
      (,) { model | currentCircle = moveCircle model.currentCircle x y } Effects.none
    AddCurrent ->
      withHashChange { model | circles = model.currentCircle :: model.circles }
    Remove circle ->
      withHashChange { model | circles = List.filter ((/=) circle) model.circles }
    ResizeCurrent dr ->
      (,) { model | currentCircle = resizeCircle model.currentCircle dr } Effects.none
    Noop ->
      (model, Effects.none)


keyDown : Int -> Signal Bool
keyDown c =
  Signal.filter identity False (Signal.sampleOn (fps 10) (Keyboard.isDown c))


inputs : List (Signal Action)
inputs =
  let
    offsetBy (x1, y1) (x2, y2) = (x2 - x1, y2 - y1)
    imageOffset = Signal.map (offsetBy (780, 680)) Window.dimensions
  in
    [ Signal.map LoadCircles History.hash
    , Signal.map MoveCurrent (Signal.map2 offsetBy imageOffset Mouse.position)
    , Signal.map (always (ResizeCurrent 5)) (keyDown 221)
    , Signal.map (always (ResizeCurrent -5)) (keyDown 219)
    ]


-- VIEW


toPx : Int -> String
toPx val =
  toString val ++ "px"


renderCircle : Signal.Address Action -> Bool -> Circle -> Html
renderCircle address isCurrent circle =
  div [
    onClick address (if isCurrent then AddCurrent else Remove circle)
  , class (if isCurrent then "" else "hair")
  , style
    [ ("background-image", "url(/images/hair.jpg)")
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
    [ ("background-image", "url(/images/bald.jpg)")
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
