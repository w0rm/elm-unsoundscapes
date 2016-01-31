import Unsoundscapes exposing (init, update, view, inputs, Model)
import StartApp exposing (App)
import Html exposing (Html)
import Task
import Effects exposing (Never)


app : { html : Signal Html
      , model : Signal Model
      , tasks : Signal (Task.Task Never ())
      }
app =
  StartApp.start
    { init = init 50 locationHash
    , update = update
    , view = view
    , inputs = inputs
    }


main : Signal Html
main =
  app.html


port locationHash : String


port tasks : Signal (Task.Task Never ())
port tasks =
  app.tasks
