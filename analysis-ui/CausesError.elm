
module Hello exposing (..)


import Html exposing ( text
                     , div
                     , textarea
                     , h1
                     , button
                     , form
                     , header
                     , body
                     , p
                     )
import Html.Attributes exposing ( style
                                , placeholder
                                , cols
                                , rows
                                )
import Html.Events exposing ( onInput
                            )
import Http
import Json.Decode as Decode
import Debug


-- | Types

type alias Model =
    { program : String
    , report  : String
    }

type Msg
    = Program String
    | Analysis
    | Report (Result Http.Error String)


-- | Model

handleResponse : Result Http.Error a -> Msg
handleResponse resp = Report resp

doAnalysis : String -> Cmd Msg
doAnalysis code =
    let url = "http://localhost:8080/scheme/analysis"
    in Debug.log "Here!"
       Http.send  ( Http.post
              { method = "POST"
              , headers = [ Http.header "Content-Type" "application/json" ]
              , url = url
              , expect = Http.expectJson
              }
              Http.emptyBody
              (Decode.list Decode.string)
        )


update : Msg -> Model -> (Model, Cmd Msg)
update msg model
    = case msg of
          Program p ->
              ({ model | program = p }, Cmd.none)
          Analysis ->
              (model, doAnalysis model.program)
          Report (Ok reportString) ->
              ({ model | report = reportString }, Cmd.none)
          Report (Err err) ->
              (model, Cmd.none)

-- | Subscriptions

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none

-- | Views

textInput : Html.Html Msg
textInput =
    div [  ]
        [ textarea [ placeholder "Enter program"
                   , onInput (\s -> Analysis)
                   , cols 80
                   , rows 40
                   , style [ ("resize", "vertical") ]
                   ] [ ]
        ]

mainPage : Model -> Html.Html Msg
mainPage model =
    let leftCol  = div [  ] [  ]
        rightCol = div [  ]
                       [
                       ]
    in div [ ]
           [ leftCol
           , div [  ]
               [ header [  ]
                     [ h1 [  ]
                          [ text "Enter Program" ]
                     ]
               , body [  ]
                   [ form [  ]
                         [ textInput
                         , button [  ] [ text "Program Analysis" ]
                         ]
                   , p [  ]
                       [ text model.report ]
                   ]
               ]
           , rightCol
           ]

view : Model -> Html.Html Msg
view model =
    mainPage model


-- | Main

init : (Model, Cmd Msg)
init =
    ( initialModel
    , Cmd.none
    )

initialModel : Model
initialModel =
    { program = ""
    , report = "Analysis will be here"
    }

main =
  Html.program { init = init
               , view   = view
               , update = update
               , subscriptions = subscriptions
               }
