
module Hello exposing (..)

import Report.Report exposing (..)
import Report.View exposing (..)

import Html exposing ( text
                     , div
                     , textarea
                     , h1
                     , form
                     , header
                     , body
                     , p
                     , br
                     , button
                     , li
                     , ul
                     )
import Html.Attributes exposing ( style
                                , placeholder
                                , cols
                                , rows
                                )
import Html.Events exposing ( onInput
                            , onClick
                            )
import Json.Decode exposing ( Decoder
                            , int
                            , string
                            , list
                            , oneOf
                            , succeed
                            )
import Json.Decode.Pipeline exposing ( decode
                                     , required
                                     )
import Json.Encode as Encode
import Http



-- | Types

type alias Model =
    { program : String
    , reportModel  : ReportModel
    }

updateWithNewReport : Model -> ReportModel -> Model
updateWithNewReport model newReport =
    { model | reportModel = newReport }

type Msg
    = Program String
    | Analysis
    | ReportMsg ReportMsg


-- | Model

update : Msg -> Model -> (Model, Cmd Msg)
update msg model
    = case msg of
          Program p ->
              ({ model | program = p }, Cmd.none)
          Analysis ->
              (model, doAnalysis model.program |> Cmd.map ReportMsg)
          ReportMsg reportMsg ->
              Report.Report.update
                  (updateWithNewReport model)
                  ReportMsg
                  reportMsg
                  model.reportModel

-- | Subscriptions

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


-- | Views

textInput : Html.Html Msg
textInput =
    div [  ]
        [ textarea [ placeholder "Enter program"
                   , cols 80
                   , rows 40
                   , onInput Program
                   , style [ ("resize", "vertical")
                           , ("float", "left")
                           ]
                   ] [ ]
        ]

mainPage : Model -> Html.Html Msg
mainPage model =
    div [ ]
        [ div [ ]
              [ header [  ]
                    [ h1 [  ]
                          [ text "Enter Program" ]
                    ]
              ]
        , div [ style [ ( "float", "left" )
                      , ( "width", "50%" )
                      ]
              ] [ body [  ]
                      [ form [  ]
                            [ textInput
                            ]
                      , button [ onClick Analysis
                               ] [ text "hello" ]
                      ]

                ]
        , div [ style [ ( "float", "left" ) ]
              ] [ p [  ]
                      [ viewReport ReportMsg model.reportModel
                      ]
                ]
        , div [ style [ ( "clear", "both" ) ]
              ] [ ]
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
    , reportModel = initialReportModel
    }

main =
  Html.program { init = init
               , view   = view
               , update = update
               , subscriptions = subscriptions
               }
