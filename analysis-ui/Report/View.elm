
module Report.View exposing ( viewReport )

import Report.Report exposing (..)

import Html exposing ( text
                     , p
                     , pre
                     , ul
                     , li
                     , div
                     , h1
                     , code
                     )
import String exposing ( lines
                       , join
                       )
import List exposing ( range
                     , map2
                     , map
                     )
import Html.Attributes exposing ( style
                                )

errorToString : SchemeError -> String
errorToString (SchemeError error line col) =
    "At line " ++ line ++ ", column " ++ col ++ ": " ++ error

-- viewLineNumber : Int -> Html msg
-- viewLineNumber num =
--     pre [ style [ () ] ]

-- viewSourceCode : String -> String
viewSourceCode (SourceCode srcCode) =
    let codeLines = lines srcCode
    in
        map2 (,) codeLines (range 1 (List.length codeLines))
            |> map (\(l, ln) -> (l, toString ln ++ "| " ++ l))
            |> join "\n"
            |> text
            |> \c -> pre [ ] [ c ]




-- viewSourceCode : SourceCode -> Html.Html msg
-- viewSourceCode (SourceCode raw) =
--     case String.length raw of
--         0 ->
--             text ""
--         _ ->
--             text (preprocessCode raw)

viewReport : (ReportMsg -> msg) -> ReportModel -> Html.Html msg
viewReport converter model =
    let reportBody ls = div
                        [ ]
                        ([ h1 [ ] [ text "What the fuck" ]
                         , pre [ style [ ("font-family", "monospace") ]
                               ] [ viewSourceCode model.sourceCode ] ] ++ ls)
    in
        case model.report of
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
                    reportBody [ ul [ style [ ] ]
                                     (List.append suggestionView errorsView)
                               ]
            (ParseError error line column) ->
                let errorText =
                        "At line " ++ line ++ ", column " ++ column ++ ": " ++ error
                in reportBody [ ul [  ]
                                    [ p [ ] [ text errorText ] ]
                              ]
