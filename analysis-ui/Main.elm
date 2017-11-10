
module Hello exposing (..)


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
import Http
import Debug

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


-- | Types

type alias Model =
    { program : String
    , report  : Report
    }

type Msg
    = Program String
    | Analysis
    | NewReport (Result Http.Error Report)

type SchemeError
    = SchemeError String String String

type Suggestion
    = Suggestion String

type Report
    = Report (List Suggestion) (List SchemeError)
    | ParseError String String String


-- | Model

codeEncoder : String -> Encode.Value
codeEncoder code =
    let attributes =
            [ ("code", Encode.string code) ]
    in Encode.object attributes

suggestionDecoder : Decoder (List Suggestion)
suggestionDecoder =
    decode (\ls -> List.map Suggestion ls)
        |> required "suggestions" (list string)

errorDecoder : Decoder SchemeError
errorDecoder =
    decode SchemeError
        |> required "error" (string)
        |> required "line" (string)
        |> required "column" (string)

parseErrorDecoder : Decoder Report
parseErrorDecoder =
    decode ParseError
        |> required "error" string
        |> required "line" string
        |> required "column" string

reportDecoder : Decoder Report
reportDecoder =
    decode (\ss es -> Report (List.map Suggestion ss) es)
        |> required "suggestions" (list string)
        |> required "errors" (list errorDecoder)

doAnalysis : String -> Cmd Msg
doAnalysis code =
    let url = "http://localhost:8080/scheme/analysis/"
    in Http.send NewReport
        ( Http.post
              url
              (Http.jsonBody (codeEncoder code))
              (oneOf [ reportDecoder
                     , parseErrorDecoder
                     ])
        )

errorToString : SchemeError -> String
errorToString (SchemeError error line col) =
    "At line " ++ toString line ++ ", column " ++ toString col ++ ": " ++ error

update : Msg -> Model -> (Model, Cmd Msg)
update msg model
    = case msg of
          Program p ->
              ({ model | program = p }, Cmd.none)
          Analysis ->
              (model, doAnalysis model.program)
          NewReport (Ok report) ->
              Debug.log (toString report) ({ model | report = report}, Cmd.none)
          NewReport (Err err) ->
              ({ model | report = Report [ ] [ ]}, Cmd.none)

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

reportView : Report -> Html.Html Msg
reportView report =
    case report of
        (Report suggestions errors) ->
            let errorsStrings = List.map errorToString errors
                errorsView =
                    errorsStrings
                        |> List.map (\s -> li [ ] [ text s ])
                suggestionView =
                    suggestions
                        |> List.map (\(Suggestion s) -> s)
                        |> List.map (\s -> li [ ] [ text s ] )
            in
                ul [ style [ ] ]
                    (List.append suggestionView errorsView)
        (ParseError error line column) ->
            let errorText =
                    "At line " ++ toString line ++ ", column " ++ toString column ++ ": " ++ error
            in ul [  ]
                  [ p [ ] [ text errorText ] ]

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
                      [ reportView model.report
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
    , report = Report [ ] [ ]
    }

main =
  Html.program { init = init
               , view   = view
               , update = update
               , subscriptions = subscriptions
               }
