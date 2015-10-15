module Unsoundscapes (inputs, init, update, view, Model) where

import Effects exposing (Effects)
import Html exposing (img, div, text, Html)
import Html.Attributes exposing (style, src)
import Html.Events exposing (onClick)
import Keyboard
import Mouse
import Signal exposing ((<~), (~), sampleOn)
import Time exposing (fps)
import Window


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
  | Offset (Int, Int)
  | SizeUp
  | SizeDown
  | Noop


update' : Action -> Model -> Model
update' action model =
  case action of
    MoveCurrent (x, y) ->
      { model | currentCircle <- moveCircle model.currentCircle x y }
    Add circle ->
      { model | circles <- model.currentCircle :: model.circles }
    Remove circle ->
      { model | circles <- List.filter ((/=) circle) model.circles }
    SizeUp ->
      { model | currentCircle <- resizeCircle model.currentCircle 5 }
    SizeDown ->
      { model | currentCircle <- resizeCircle model.currentCircle -5 }
    Noop ->
      model


update : Action -> Model -> (Model, Effects Action)
update action model =
  ((update' action model), Effects.none)


keyDown : Int -> Signal Bool
keyDown c =
  Signal.filter identity False (Signal.sampleOn (fps 10) (Keyboard.isDown c))


inputs : List (Signal Action)
inputs =
  let
    offsetBy (x1, y1) (x2, y2) = (x2 - x1, y2 - y1)
    imageOffset = offsetBy (780, 680) <~ Window.dimensions
  in
    [ MoveCurrent <~ (offsetBy <~ imageOffset ~ Mouse.position)
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
