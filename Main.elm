module Main exposing (..)

import Html exposing (img, div, text, Html)
import Html.Attributes exposing (style, src, class)
import Html.Events exposing (onClick)
import Keyboard
import Mouse
import Window
import Task
import Navigation
import String


locationToMessage : Navigation.Location -> Msg
locationToMessage =
    (.hash >> circlesFromHash >> LoadCircles)


main : Program Never Model Msg
main =
    Navigation.program
        locationToMessage
        { init =
            (\msg ->
                ( Tuple.first (update (locationToMessage msg) model)
                , Task.perform Resize Window.size
                )
            )
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


subscriptions : Model -> Sub Msg
subscriptions { size } =
    Sub.batch
        [ Window.resizes Resize
        , Mouse.moves (\{ x, y } -> MoveCurrent ( x + 780 - size.width, y + 680 - size.height ))
        , Keyboard.downs
            (\code ->
                case code of
                    221 ->
                        ResizeCurrent 5

                    219 ->
                        ResizeCurrent -5

                    _ ->
                        ResizeCurrent 0
            )
        ]



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
        |> List.concatMap (\{ x, y, r } -> [ x - r, y - r, r // 5 ])
        |> List.map toString
        |> List.intersperse ","
        |> (::) "#"
        |> List.foldr (++) ""


type alias Model =
    { currentCircle : Circle
    , circles : List Circle
    , size : Window.Size
    }


model : Model
model =
    Model (Circle 0 0 50) [] (Window.Size 0 0)



-- UPDATE


type Msg
    = Remove Circle
    | AddCurrent
    | MoveCurrent ( Int, Int )
    | ResizeCurrent Int
    | LoadCircles (List Circle)
    | Resize Window.Size


withHashChange : Model -> ( Model, Cmd Msg )
withHashChange model =
    ( model
    , Navigation.newUrl (hashFromCircles model.circles)
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LoadCircles circles ->
            { model | circles = circles } ! []

        MoveCurrent ( x, y ) ->
            { model | currentCircle = moveCircle model.currentCircle x y } ! []

        AddCurrent ->
            withHashChange { model | circles = model.currentCircle :: model.circles }

        Remove circle ->
            withHashChange { model | circles = List.filter ((/=) circle) model.circles }

        ResizeCurrent dr ->
            { model | currentCircle = resizeCircle model.currentCircle dr } ! []

        Resize size ->
            { model | size = size } ! []



-- VIEW


toPx : Int -> String
toPx val =
    toString val ++ "px"


renderCircle : Bool -> Circle -> Html Msg
renderCircle isCurrent circle =
    div
        [ onClick
            (if isCurrent then
                AddCurrent
             else
                Remove circle
            )
        , class
            (if isCurrent then
                ""
             else
                "hair"
            )
        , style
            [ ( "background-image", "url(/images/hair.jpg)" )
            , ( "background-repeat", "no-repeat" )
            , ( "border-radius", "50%" )
            , ( "position", "absolute" )
            , ( "width", toPx (circle.r * 2) )
            , ( "height", toPx (circle.r * 2) )
            , ( "left", toPx (circle.x - circle.r) )
            , ( "top", toPx (circle.y - circle.r) )
            , ( "background-position", toPx (circle.r - circle.x) ++ " " ++ toPx (circle.r - circle.y) )
            ]
        ]
        []


view : Model -> Html Msg
view model =
    div
        [ style
            [ ( "background-image", "url(/images/bald.jpg)" )
            , ( "bottom", "0" )
            , ( "height", toPx 680 )
            , ( "overflow", "hidden" )
            , ( "position", "absolute" )
            , ( "right", "0" )
            , ( "width", toPx 780 )
            ]
        ]
        (renderCircle True model.currentCircle
            :: List.map (renderCircle False) model.circles
        )
