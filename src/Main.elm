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
    , totalInfected : Int
    , t : Int
    , initialSeed : Int
    , seed : Random.Seed
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
          , initialSeed = 2343
          , totalInfected = 1
          , seed = Random.initialSeed 2343
          }
        , Cmd.none
        )


view : Model -> Html.Html Msg
view { r0, sData, iData, rData, t, initialSeed, totalInfected } =
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
            , inputSectionEl r0 initialSeed
            , viewSectionEl sData iData t s i r totalInfected
            , controlSectionEl
            ]
        )


type Msg
    = IncrementR0
    | DecrementR0
    | Step
    | Random
    | FastForward


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
            ( step model
            , Cmd.none
            )

        Random ->
            let
                seedGenerator =
                    Random.int Random.minInt Random.maxInt

                rndSeed =
                    Random.initialSeed model.initialSeed

                ( newSeedInt, _ ) =
                    Random.step seedGenerator rndSeed
            in
            ( { model | initialSeed = newSeedInt }, Cmd.none )

        FastForward ->
            ( fastForward model
            , Cmd.none
            )


step : Model -> Model
step model =
    {--explanation.
        For every time step, S I R containers
        are updated like this.

        Let dI be the number of individuals
        that will be contamined in this step.

        dI is decided by, for every individual
        in S, flipping I_old coins. If any flip
        turns up 'infected' side, dI is incremented.

        After a little thought, the probability
        to not be infected with R0 = 2.5, N = 1000
        and 500 other infected individuals:

          pInfected = (1-2.5/1000) ^ 500
          pInfected = (1-r0/1000) ^ iPrev

        Think of flipping fair coin, probability
        to not get any heads once is 1/2; 1/4 to
        not get any 2 flips, 1/8 for 3 flips and so
        on.

        So we know probability of

        The new value of S is S_new = S - dI.
        The new value of I is I_new = dI.
        The new value of R is R_new = R_old + I_old.
    --}
    let
        sPrev =
            Maybe.withDefault 0 <| List.head model.sData

        iPrev =
            Maybe.withDefault 0 <| List.head model.iData

        rPrev =
            Maybe.withDefault 0 <| List.head model.rData

        p =
            model.r0 / toFloat sPrev

        pInfected =
            (1 - p) ^ toFloat iPrev

        initialSeed =
            Random.initialSeed model.initialSeed

        -- list of random numbers 0..1 for every susceptible
        ( rndList, newSeed ) =
            Random.step (Random.list sPrev probability) initialSeed

        dI =
            List.length <| List.filter (\x -> x > pInfected) rndList

        sNew =
            sPrev - dI

        iNew =
            dI

        rNew =
            rPrev + iPrev
    in
    { model
        | sData = sNew :: model.sData
        , iData = iNew :: model.iData
        , rData = rNew :: model.rData
        , totalInfected = model.totalInfected + dI
        , t = model.t + 1
        , seed = newSeed
    }


fastForward model =
    let
        currentS =
            Maybe.withDefault 0 <| List.head model.sData

        currentI =
            Maybe.withDefault 0 <| List.head model.iData

        currentR =
            Maybe.withDefault 0 <| List.head model.rData
    in
    model


headerEl =
    El.el [ El.centerX, El.padding space ]
        (El.text "INFECTIOUS DISEASE SIMULATION")


inputSectionEl r0 initialSeed =
    El.row [ El.centerX, El.spacing space ]
        [ r0InputElement r0
        , seedInputElement initialSeed
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


seedInputElement initialSeed =
    El.row [ El.spacing 5, smallFont ]
        [ El.text "initialSeed ="
        , El.text <| String.fromInt initialSeed
        , buttonElement "random" Random
        ]


viewSectionEl sData iData t s i r totalInfected =
    El.column [ El.centerX ]
        [ El.el [ El.centerX, El.padding 5 ]
            (El.text <| "Infected: " ++ String.fromInt totalInfected)
        , El.row []
            [ chartEl sData iData
            , stateEl t s i r
            ]
        ]


chartEl sData iData =
    El.el
        [ El.width <| El.px 400
        , El.height <| El.px 200
        ]
        (El.html <| chart sData iData)


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
            , buttonElement ">>" FastForward
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


chart sData iData =
    let
        chart2point index y =
            { x = toFloat index, y = toFloat y }

        sChart =
            List.indexedMap chart2point (List.reverse sData)

        iChart =
            List.indexedMap chart2point (List.reverse iData)
    in
    LineChart.viewCustom
        { x = Axis.default 700 "t" .x
        , y = Axis.default 400 "n" .y
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
        [ LineChart.line Colors.red Dots.square "Infected" iChart
        , LineChart.line Colors.green Dots.plus "Susceptible" sChart
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
    El.rgb 1 0.5 0.5


green =
    El.rgb 0.5 1 0.5


blue =
    El.rgb 0.5 0.5 1


lightGray =
    El.rgb 0.9 0.9 0.9


space =
    20


smallFont =
    ElFont.size 12



-- @remind
-- Visualize total infected
-- Add probability for death of infected individual
--  1) when hospital bed is available
--  2) when not available
-- Visualize total deaths
-- Add hospital beds
-- Add D to SIR (death data)
-- Link to Wikipedia model page
-- Link to YouTube model introduction video
-- Brief explanation of things/legend?
