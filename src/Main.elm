module Main exposing (main)

import Browser
import Element as El
import Element.Background as ElBackground
import Element.Border as ElBorder
import Element.Font as ElFont
import Element.Input as ElInput
import Html
import LineChart
import LineChart.Area as Area
import LineChart.Axis as Axis
import LineChart.Axis.Intersection as Intersection
import LineChart.Colors as Colors
import LineChart.Container as Container
import LineChart.Dots as Dots
import LineChart.Events as Events
import LineChart.Grid as Grid
import LineChart.Interpolation as Interpolation
import LineChart.Junk as Junk
import LineChart.Legends as Legends
import LineChart.Line as Line
import Random
import Round


type alias Model =
    { r0 : Float
    , sData : List Int
    , iData : List Int
    , rData : List Int
    , t : Int
    , seed : Int
    }


main : Program () Model Msg
main =
    Browser.element
        { init = initial
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }


initial =
    \_ ->
        ( { r0 = 1.4
          , sData = [ 999 ]
          , iData = [ 1 ]
          , rData = [ 0 ]
          , t = 0
          , seed = 2343
          }
        , Cmd.none
        )


view : Model -> Html.Html Msg
view { r0, sData, iData, rData, t, seed } =
    let
        s =
            Maybe.withDefault 0 <| List.head sData

        i =
            Maybe.withDefault 0 <| List.head iData

        r =
            Maybe.withDefault 0 <| List.head rData
    in
    El.layout []
        (El.column [ El.centerX ]
            [ headerEl
            , inputSectionEl r0 seed
            , viewSectionEl sData t s i r
            , controlSectionEl
            ]
        )


type Msg
    = IncrementR0
    | DecrementR0
    | Step
    | Random


update msg model =
    case msg of
        IncrementR0 ->
            ( { model | r0 = model.r0 + 0.05 }
            , Cmd.none
            )

        DecrementR0 ->
            ( { model | r0 = model.r0 - 0.05 }
            , Cmd.none
            )

        Step ->
            ( { model
                | sData = 940 :: model.sData
                , t = model.t + 1
              }
            , Cmd.none
            )

        Random ->
            let
                seedGenerator =
                    Random.int Random.minInt Random.maxInt

                rndSeed =
                    Random.initialSeed model.seed

                ( newSeedInt, _ ) =
                    Random.step seedGenerator rndSeed
            in
            ( { model | seed = newSeedInt }, Cmd.none )


headerEl =
    El.el [ El.centerX, El.padding space ]
        (El.text "INFECTIOUS DISEASE SIMULATION")


inputSectionEl r0 seed =
    El.row [ El.centerX, El.spacing space ]
        [ r0InputElement r0
        , seedInputElement seed
        ]


r0InputElement r0 =
    El.row [ El.spacing 5 ]
        [ El.text "R0 ="
        , El.text <| Round.round 2 r0
        , if r0 > 0.05 then
            buttonElement "-" DecrementR0

          else
            El.text "-"
        , buttonElement "+" IncrementR0
        ]


seedInputElement seed =
    El.row [ El.spacing 5, smallFont ]
        [ El.text "Seed ="
        , El.text <| String.fromInt seed
        , buttonElement "random" Random
        ]


viewSectionEl sData t s i r =
    El.row [] [ chartEl sData, stateEl t s i r ]


chartEl sData =
    El.el
        [ El.width <| El.px 400
        , El.height <| El.px 200
        ]
        (El.html <| chart sData)


stateEl t s i r =
    El.column [ El.alignTop ]
        [ sirStateEl s i r
        , El.el [ El.centerX ] (El.text <| "t = " ++ String.fromInt t)
        ]


sirStateEl s i r =
    El.row []
        [ bar "S" s green
        , bar "I" i red
        , bar "R" r blue
        ]


bar letter count color =
    El.column
        []
        [ El.el
            [ ElBackground.color color
            , El.width <| El.px 20
            , El.height <| El.px 200
            , El.alignBottom
            , El.centerX
            , El.padding 5
            ]
            (El.el
                [ El.centerX, El.alignBottom ]
                (El.text letter)
            )
        , El.el
            [ El.centerX, El.alignBottom, smallFont ]
            (El.text <|
                String.fromInt count
            )
        ]


controlSectionEl =
    El.column
        [ El.centerX
        , El.padding space
        , El.spacing space
        , ElBorder.width 1
        ]
        [ El.el [ El.centerX ] <| buttonElement "Step" Step
        , El.row [ El.spacing space ]
            [ El.text "<<"
            , El.text ">"
            , El.text ">>"
            ]
        ]


type alias Point =
    { x : Float, y : Float }


probability : Random.Generator Float
probability =
    Random.float 0 1


randomPoint : Random.Generator Point
randomPoint =
    Random.map2
        Point
        probability
        probability


randomChart : Random.Generator (List Point)
randomChart =
    Random.int 3 10
        |> Random.andThen (\len -> Random.list len randomPoint)


chart : List Int -> Html.Html msg
chart sData =
    let
        seed1 =
            Random.initialSeed 1003

        seed2 =
            Random.initialSeed 999

        ( iChart, _ ) =
            Random.step randomChart seed1

        chart2point index y =
            { x = toFloat index, y = toFloat y }

        sChart =
            List.indexedMap chart2point (List.reverse sData)
    in
    LineChart.viewCustom
        { x = Axis.default 700 "X" .x
        , y = Axis.default 400 "Y" .y
        , container = Container.responsive "line-chart-1"
        , interpolation = Interpolation.default
        , intersection = Intersection.default
        , legends = Legends.default
        , events = Events.default
        , junk = Junk.default
        , grid = Grid.default
        , area = Area.default
        , line = Line.default
        , dots = Dots.default
        }
        [ LineChart.line Colors.green Dots.square "Infected" iChart
        , LineChart.line Colors.red Dots.plus "Susceptible" sChart
        ]



-- UTILITY


buttonElement : String -> Msg -> El.Element Msg
buttonElement buttonText onPressMsg =
    El.el
        [ ElBackground.color lightGray
        , El.alignLeft
        ]
        (ElInput.button
            [ El.padding 5
            ]
            { onPress = Just onPressMsg
            , label = El.text buttonText
            }
        )



-- PALETTE


red =
    El.rgb 1 0 0


green =
    El.rgb 0 1 0


blue =
    El.rgb 0 0 1


lightGray =
    El.rgb 0.9 0.9 0.9


space =
    20


smallFont =
    ElFont.size 12



-- @remind
-- Link to Wikipedia model page
-- Link to YouTube model introduction video
-- Brief explanation of things/legend?
