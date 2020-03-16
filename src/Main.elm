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
    { r0 : Float }


type Msg
    = IncrementR0
    | DecrementR0


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> ( { r0 = 1.4 }, Cmd.none )
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }


view : Model -> Html.Html Msg
view { r0 } =
    El.layout []
        (El.column [ El.centerX ]
            [ headerEl
            , inputSectionEl r0
            , viewSectionEl
            , controlSectionEl
            ]
        )


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


headerEl =
    El.el [ El.centerX, El.padding space ]
        (El.text "INFECTIOUS DISEASE SIMULATION")


inputSectionEl r0 =
    El.row [ El.centerX, El.spacing space ]
        [ r0InputElement r0
        , seedInputElement
        ]


r0InputElement r0 =
    El.row [ El.spacing 5 ]
        [ El.text "R0"
        , El.text <| Round.round 2 r0
        , if r0 > 0.05 then
            buttonElement "-" DecrementR0

          else
            El.text "-"
        , buttonElement "+" IncrementR0
        ]


seedInputElement =
    El.row [ El.spacing 5, smallFont ]
        [ El.text "Seed"
        , El.text "329847"
        , El.text "random"
        ]


viewSectionEl =
    El.row [] [ chartEl, stateEl ]


chartEl =
    El.el
        [ El.width <| El.px 400
        , El.height <| El.px 200
        ]
        (El.html chart)


stateEl =
    El.column [ El.alignTop ]
        [ sirStateEl
        , El.el [ El.centerX ] (El.text "t=5")
        ]


sirStateEl =
    El.row []
        [ bar "S" 870 green
        , bar "I" 30 red
        , bar "R" 100 blue
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
        [ El.el [ El.centerX ] (El.text "Step")
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


chart : Html.Html msg
chart =
    let
        seed1 =
            Random.initialSeed 1003

        seed2 =
            Random.initialSeed 999

        ( chart1, _ ) =
            Random.step randomChart seed1

        ( chart2, _ ) =
            Random.step randomChart seed2
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
        [ LineChart.line Colors.blueLight Dots.square "Chuck" chart1
        , LineChart.line Colors.pinkLight Dots.plus "Alice" chart2
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
