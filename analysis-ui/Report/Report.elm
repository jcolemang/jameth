
module Report.Report exposing ( initialReportModel
                              , doAnalysis
                              , update
                              , Report (..)
                              , SourceCode (..)
                              , ReportModel
                              , ReportMsg
                              , SchemeError (..)
                              , Suggestion (..)
                              )

import Html exposing ( text
                     )
import Json.Decode exposing ( oneOf
                            , list
                            , string
                            , Decoder
                            )
import Json.Decode.Pipeline exposing ( decode
                                     , required
                                     )
import Http
import Json.Encode as Encode

type alias ReportModel
    = { report     : Report
      , sourceCode : SourceCode
      }

type SourceCode =
    SourceCode String

type ReportMsg
    = NewReport (Result Http.Error Report) SourceCode

type Report
    = Report (List Suggestion) (List SchemeError)
    | ParseError String String String

type SchemeError
    = SchemeError String String String

type Suggestion
    = Suggestion String

initialReportModel : ReportModel
initialReportModel =
    { report = Report [ ] [ ]
    , sourceCode = SourceCode ""
    }

-- | Update

update :  (ReportModel -> model)
       -> (ReportMsg -> msg)
       -> ReportMsg
       -> ReportModel
       -> (model, Cmd msg)
update modelConverter msgConverter msg model =
    case msg of
        NewReport (Ok report) source ->
            let newModel =
                    { model
                        | report     = report
                        , sourceCode = source
                    }
                cmd = Cmd.none
            in
                (modelConverter newModel, cmd)
        NewReport (Err err) _ ->
            let newModel =
                    { model | report = Report [ ] [ ]
                    }
                cmd = Cmd.none
            in
                (modelConverter newModel, cmd)

-- | Decoding

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

doAnalysis : String -> Cmd ReportMsg
doAnalysis code =
    let url = "http://localhost:8080/scheme/analysis/"
    in Http.send (\rprt -> NewReport rprt (SourceCode code))
        ( Http.post
              url
              (Http.jsonBody (codeEncoder code))
              (oneOf [ reportDecoder
                     , parseErrorDecoder
                     ])
        )
