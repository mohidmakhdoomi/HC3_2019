{- 
TrigVis

Group 31
Authors:
    Mohid Makhdoomi - 400021935
    Abdul Moiz - 400017302
    Amit Binu - 400023175
    Mohamed Bengezi - 400021279

The problem our group tried to solve is how to help first year university students with visualizing
and understanding the concepts behind the trigonometric functions they learn in math classes.
To do this we first created our paper prototype, and then based on feedback made the final 
prototype and in the end, based on further feedback, developed this final code implementation.

The key features of this project (TrigVis) are as follows:
1. Graph section which has a grid, axes, labels on x and y axes and
    allows for the display of graphical representations of trig equations
2. Functions section which has 6 trig functions listed and 
    '+' buttons to add new equations to the model
3. Equations section which display's trig equations as colored text,
    allows for selecting an equation by clicking on it and also
    'x' buttons to remove equations from the section and from the model
4. Parameters section which provides 4 sliders that allow for adjusting
    a selected wave's Amplitude, Period, Phase Shift and Vertical Shift

How we imagine TrigVis to used is by adding trig equations, adjusting parameters and then observing
real time changes to the graph. Our hope is that through this very interactive and visual experience
our users are able to better comprehend and make sense of trigonometric functions.

Some important things to note about TrigVis:
- By default, we start with 0 existing equations and so the Graph is empty,
    the sliders in the Parameters section is disabled and there are
    no equations displayed in the Equations section
- The model limits the user to have up to 6 trig equations at once
- The model limits the sliders in the Parameters section as follows:
    Amplitude: min = -0.5 , max = 2.5
    Period: min = 0 , max = 2
    Phase Shift: min = -1*pi , max = 1*pi
    Vertical Shift: min = -2 , max = 2
- Once the user adds a trig equation to the model through the Functions 
    section, the equation is displayed in a specific color in the 
    Equations section, its graphical representation is added to the 
    Graph section with the same color and the Parameter section is enabled.
- Whenever an equation is added it has the following default values:
    Amplitude = 1, Period = 1, Phase Shift = 0*pi, Vertical Shift = 0
    and the newly added equation becomes the currently selected equation
- Whenever an equation is removed through clicked the 'x' button in the
    Equations section, its textual and graphical representation are both
    removed. If the removed equation was the currently selected equation
    then the equation above it becomes the new selected equation.
    If all equations have been removed then the Parameters section 
    again becomes disabled
- An equation can be selected in the following ways:
    1. Through the user adding it from the Functions section
    2. Clicking it in the Equations section
    3. Removing the equation directly after it, but only if the removed
        equation was the currently selected equation
- Whenever an equation is selected, its textual and graphical representations
    become bolded and its trigonometric parameters are loaded onto the sliders
    in the Parameters section. 
- Whenever an equation is selected, adjusting the sliders updates both textual and 
    graphical representations almost immediately.

FROM THIS POINT FOWARD: 
    the graphical representation of a trig equation will also be referred to as its wave
-}

module TrigVis exposing (..)


import Array exposing (..)
import GraphicSVG exposing (..)
import GraphicSVG.EllieApp exposing (..)
import List


main =
    gameApp Tick
        { model = init
        , title = "TrigVis"
        , view = view
        , update = update
        }


init = 
    { currWave = 0  -- integer value of currently selected wave (1 indexed). Default value of 0 to represent that there are currently no equations
    , waves = []  -- This list holds all trig equations (type alias Wave), where Wave contains all needed information about each equation
    , editing = False  -- boolean value of whether sliders are currently being used, defaults to False
    
    , maxWaves = 6  -- maximum number of allowed equations is set to 6
    , allColors = [(rgb 127 0 255), (rgb 255 0 0), (rgb 0 185 0), (rgb 0 0 0), (rgb 190 190 0), (rgb 0 127 255)]
    , usedColors = []
    -- above two variables are used for choosing the colors of equations and their graphical representation (wave)

    -- through the use of sliders, we do not need to use constants to set max values
    -- instead, we scale 0 to 100 values of the slider to match the required range for each parameter
    -- this becomes more clear in later comments and code
    , maxWaveLength = 400  -- maximum width that the graphical representation of each equation can use
    }


-- user message type Msg which takes one argument m, which is the user persistent model
type Msg m
    = Tick Float GetKeyState  -- Message handler called once per browser window update ~30 times a second
    | TransM (m -> m)  -- Simply takes a function and applies it to the model. The function must take a model as input and give a model as output
    | Slider Parameters (Float,Float) (Float,Float)
    | StartSlider Parameters (Float,Float) (Float,Float)
    | EndSlider -- Simply sets the 'editing' variable equal to False to represent that the user is not using the sliders
    -- Slider and StartSlider both take 3 arguements, first is of type Parameter (i.e. Amplitude, Period, etc.), second and third are coordinates.
    -- First coordinate is the (x, y) location of the slider on the canvas, second coordinate is the (x, y) location of the cursor
    -- Both Slider and StartSlider are responsible for capturing the user adjusting the slider and also for then updating the value of
    -- the given Parameter (which is the first argument), for the currently selected equation


-- types of messages recieved for certain mouse/cursor interactions with Shapes 
type Notifications
    = NotifyTap
    | NotifyTapAt
    | NotifyEnter
    | NotifyEnterAt
    | NotifyLeave
    | NotifyLeaveAt
    | NotifyMouseMoveAt
    | NotifyMouseDown
    | NotifyMouseDownAt
    | NotifyMouseUp
    | NotifyMouseUpAt
    | NotifyTouchStart
    | NotifyTouchStartAt
    | NotifyTouchEnd
    | NotifyTouchEndAt
    | NotifyTouchMoveAt


-- 6 possible type of trig functions that the equations can use
type Trig
    = Sin
    | Cos
    | Tan
    | Csc
    | Sec
    | Cot


-- the 4 parameters of the equations that can be adjusted
type Parameters
    = Amplitude  -- distance from rest to crest of a wave
    | Period  -- time it takes to complete once cycle of a wave but this is only a coefficent (used as denominator in 2*pi/x)
    | PhaseShift  -- horizontal shift of a wave
    | VerticalShift  -- vertical shift of a wave


-- type that represents an individual trig equation and the attributes/parameters of its wave
type alias Wave = 
    { uArg : Float  -- impacted by changes to the period and is used to calculate the y position of the wave
    , uScale : Float  -- how we adjust the amplitude
    , uDilation : Float -- how we adjust the period
    , uPhaseShift : Float  -- how we adjust the phase shift
    , uVertShift : Float  -- how we adjust the vertical shift
    , pointGraph : List (Float, Float)  -- list of points that make up all points used to construct the wave
    , trigCycleU : Trig  -- the trig function that generates this wave
    , uGraph : Float  -- the y position of the wave before any shifts, calculated using trigCycleU, uArg and uScale
    , uColor : GraphicSVG.Color  -- the color of the wave, also used for its textual representation
    }


-- "Helper function to update the state of the widget" - directly from https://package.elm-lang.org/packages/MacCASOutreach/graphicsvg/7.1.0/
-- This functions purpose is to update the model
update msg model =
    case msg of
        -- every tick/update of browser: update all waves in the model
        Tick _ _ -> 
            { model
                | waves =
                    List.map (waveUpdate model.maxWaveLength) model.waves
                        
            }
        -- figure out the value the user is setting on the slider and update the current wave accordingly
        Slider param (ox,_) (cx,_) ->
            let
                x = cx - ox  -- (cursor coordinate) - (origin coordinate of the slider shape) = relative x value of cursor

                -- using x, determine the value selected on the slider
                value = 
                    ( if x < -50 then 
                        0
                    
                    else if x > 50 then
                        100
                    
                    else
                        x + 50
                    )

                cw = model.currWave

                -- map over list of waves and for the current wave update it using the new value from the slider
                newWaves =
                    if model.editing then
                        List.indexedMap
                            (\idx wav ->
                                if (idx + 1) == cw then
                                    waveParameters wav value param

                                else
                                    wav
                            )
                            model.waves
                    else
                        model.waves                
            in      
            { model | waves = newWaves }
        -- same functionality as Slider above except this also sets editing = True, indicating that the user
        -- has started using the slider
        StartSlider param (ox,_) (cx,_) ->
            let
                x = cx - ox

                value = 
                    ( if x < -50 then
                        0

                    else if x > 50 then
                        100

                    else
                        x + 50
                    )

                cw = model.currWave

                newWaves =
                    List.indexedMap
                        (\idx wav ->
                            if (idx + 1) == cw then
                                waveParameters wav value param

                            else
                                wav
                        )
                        model.waves
            in  
            { model | waves = newWaves, editing = True }
        -- EndSlider simply sets editing equal to False to indicate the user is no longer using the slider
        EndSlider -> { model | editing = False } 
        -- TransM takes a function t and passes it the user persistent model,
        -- the result being an updated state of the model
        TransM t ->
            t model


-- "Helper function which takes the widget's opaque model as well as a list of shapes to include in the widget." - directly from https://package.elm-lang.org/packages/MacCASOutreach/graphicsvg/7.1.0/
-- This functions purpose is to take the model and all our shapes and display them
view model =
    -- canvas of 512 width, 380 height
    collage 512 380 <|
        let
            numWaves = List.length model.waves

            -- text labels for all of our trig functions
            setOfFunctions = 
                group
                    [ funcLabel Sin 1 -2.25
                    , funcLabel Cos 1 -22.25
                    , funcLabel Tan 1 -42.25
                    , funcLabel Csc 1 -62.25
                    , funcLabel Sec 1 -82.25
                    , funcLabel Cot 1 -102.25
                    ]

            -- + sign Shapes for all of our trig functions
            setOfPlus = 
                [ (addFunc -10 0, Sin)
                , (addFunc -10 -20, Cos)
                , (addFunc -10 -40, Tan)
                , (addFunc -10 -60, Csc)
                , (addFunc -10 -80, Sec)
                , (addFunc -10 -100, Cot)
                ]
                
            -- little circle that sits at x = 0 and moves with a waves y values as they are produced
            -- purpose is to nicely show the current values of each wave
            circleGraphics wave cw idx =
                let
                    thick = (if cw == (idx + 1) then 1.5 else 1)
                in                   
                circle thick |> filled wave.uColor |> move ( 0, wave.uGraph )
        in
        [ graphPaperCustom 20 0.5 (rgb 5 137 255) |> makeTransparent 0.25 -- generates the grid for Graph section
        , group
            [ rect 530 0.5 |> filled blue |> move (200, 0)
            , rect 0.5 1500 |> filled blue
            , ( group <|
                ( if numWaves > 0 then
                    -- the below indexedMap maps the anonymous function to each wave in our model
                    -- the anonymous function takes a wave with all its attributes and points,
                    -- generates a GraphicSVG Shape and also does any vertical shift needed
                    List.indexedMap 
                        (\idx wav ->
                            group 
                                [ group (pointsToCurve wav.pointGraph wav.uColor model.currWave idx)  -- trig equation points to Shape
                                , circleGraphics wav model.currWave idx
                                ] |> move (0, wav.uVertShift)  -- apply vertical shift
                        )
                        model.waves
                else
                    []
                )
            ) |> scale 2
            ] |> move ( -200, 80 )
        -- lightBlue background for Functions, Equations and Parameters sections
        , rect 510 158.5 |> filled (rgb 205 225 255) |> addOutline (solid 1.1) lightBlue |> move (0, -110)  
        , group
            [ setOfFunctions
            , group <|
                -- below List.map takes our + signs, maps a notifyTap notification to it which updates the model by adding the new trig function 
                -- if we have reached the maximum waves, it removes the above functionality and makes the shape 
                -- more transparent to indicate the 'button' is disabled
                ( List.map
                    (\(plus, func) ->
                        ( if numWaves < model.maxWaves then
                            plus |> notifyTap (TransM (addWave func))
                        
                        else
                            plus |> makeTransparent 0.5
                        )
                    )
                    setOfPlus
                )              
            ] |> move (-218, -70)
        , (group <|
            -- Below creates an 'x' sign for each wave, which when clicked updates the model by removing the specific wave from the model
            ( List.map
                (\idx ->
                    removeFunc 20 -(20 * (Basics.toFloat idx))
                        |> notifyTap (TransM (removeWave idx))
                )
                (List.range 1 (List.length model.waves))
            )
            ++ 
            List.indexedMap (functionText model) model.waves
        ) |> move (-180, -50)
        -- Below line generates all 4 of our sliders, it also makes them more transparent if there are no waves, to indicate sliders are disabled
        , setOfSlider model (195,-73) 30 |> makeTransparent (if numWaves > 0 then 1 else 0.5)
        , rect 1.1 140 |> filled lightBlue |> move (-180,-110)
        , rect 1.1 140 |> filled lightBlue |> move (73,-110)
        , group <|
            -- Turns our labels for the Functions, Equations and Parameters sections into shapes
            ( List.indexedMap 
                (\i txt ->
                    txt |> size 10 |> bold |> centered |> underline |> filled darkBlue
                        |> move ((if i == 0 then -218 else if i == 1 then -53.5 else 164.5), -47.5)
                )
                [text "Functions", text "Equations", text "Parameters"]
            )
        -- generates a Shape consisting of our y axis labels
        , group <|
            ( List.map
                (\idx ->
                    text (String.fromInt idx) |> size 6 |> alignRight |> fixedwidth |> filled black |> move (-202.5, 78.4 + (20 * (Basics.toFloat idx)) - (if idx == 0 then 4.5 else 0))
                )
                (List.range -5 5)
            )
        -- generates a Shape consisting of our x axis labels
        , ( group <|
            ( List.map
                (\idx ->  -- gridToPiString does some interesting stuff to generate text with the pi symbol and fractions
                    gridToPiString idx |> size 6 |> centered |> fixedwidth |> filled black |> move (20 * Basics.toFloat idx, 73.9)
                )
                (List.range 1 22)
            ++
            -- below filterMap applies an anonmyous function to filter out all values that do not need a 2 in the denominator
            List.filterMap  
                (\idx -> 
                    ( if (remainderBy 2 idx) /= 0 then 
                        -- the below line simply writes the number 2 in the denominator where ever fractions occur on the x axis
                        Just (text "2" |> size 6 |> centered |> fixedwidth |> filled black |> move (20 * Basics.toFloat idx, 68.4))
                        
                    else
                        Nothing
                    )
                )
                (List.range 1 22)
            )
        ) |> move (-200, 0)
        ]


-- helper functions starting below


-- takes a type Trig and a value u and returns the corresponding value of that mathematical trig function when given u
evalTrig f u =
    case f of
        Sin ->
            sin u

        Cos ->
            cos u

        Tan ->
            tan u

        Csc ->
            1/(sin u)

        Sec ->
            1/(cos u)

        Cot ->
            1/(tan u)


-- takes a type Trig and returns a string representation of it
textTrig f =
    case f of
        Sin ->
            "sin"

        Cos ->
            "cos"

        Tan ->
            "tan"

        Csc ->
            "csc"

        Sec ->
            "sec"

        Cot ->
            "cot"


-- takes a type Parameter and returns a string representation of it
textParam p =
    case p of
        Amplitude ->
            "Amplitude"

        Period ->
            "Period"

        PhaseShift ->
            "Phase Shift"

        VerticalShift ->
            "Vertical Shift"



-- just the text labels for trig functions in bottom left of screen
funcLabel f x y =
    text (textTrig f) |> size 10 |> filled black |> move (x, y)


-- Green add trig function Shape
addFunc x y =
    group
        [ circle 6 |> filled green
        , rect 2 7.5 |> filled white
        , rect 2 7.5 |> filled white |> rotate (degrees 90)]
        |> move (x, y)


-- Red remove trig function Shape
removeFunc x y =
    group 
        [ circle 6 |> filled red
        , rect 2 7.5 |> filled white |> rotate (degrees 45)
        , rect 2 7.5 |> filled white |> rotate (degrees -45) ]
        |> move (x, y)


-- takes a list of the waves points, the waves color, the index of the models current wave, and the index of passed wave
-- Basically takes all the waves points and draws a line between each pair of adjacent points
-- if the passed wave is the models current wave, then make its Shape outline thicker
pointsToCurve funcPoints waveColor cw idx =
    let
        thick = (if cw == (idx + 1) then 1 else 0.5)

        points =
            List.map2 (\x y -> ( x, y )) funcPoints (List.drop 1 funcPoints)

        fqn = 
            \( ( a, b ), ( c, d ) ) -> line ( a, b ) ( c, d ) |> outlined (solid thick) waveColor
    in
    List.map fqn points


-- for passed wave, update the waves points and attributes/properties
waveUpdate maxWaveLength wave =
    let
        -- uArg is the value passed to the mathematical trig function, so based on uDilation (Period) we
        -- adjust uArg so that we can represent changes in the waves period
        uArg =
            wave.uArg + wave.uDilation * (0.0055)

        -- latest y value of the wave using the updated uArg
        uGraph =
            wave.uScale * evalTrig wave.trigCycleU uArg
            
        -- the above point as an x, y coordinate
        firstPoint =
            ( 0, uGraph )

        tmpPoints =
            List.take 650  -- use only the latest 650 points
                ([ firstPoint ]
                    ++ List.filterMap
                        (\( xx, yy ) ->
                            if xx >= maxWaveLength then
                                Nothing

                            else
                                -- essentially increment all previous x values by 0.35 to move the wave along as new points are generated
                                Just ( xx + 0.35, yy )  
                        )
                        wave.pointGraph
                )
    in
        -- return the updated wave with its new points, new uArg and new uGraph values
        { wave | pointGraph = tmpPoints, uArg = uArg, uGraph = uGraph }


-- given a Trig type and the color for the wave, generate a new Wave record with default parameters
template func wColor =
    { uArg = 0, uScale = 10.00, uDilation = 10.00, uPhaseShift = 0.00, uVertShift = 0.00, pointGraph = [], trigCycleU = func, uGraph = 0, uColor = wColor }


-- add new wave to model based on provided Trig function
addWave f m =
    let
        ps = m.waves

        maxWaves = m.maxWaves

        cw = List.length m.waves

        cols = m.allColors

        usedCols = m.usedColors

        -- take the first available color from the models allColors and set it as the new waves color
        newColor = 
            case (List.head cols) of
                Nothing -> 
                    black

                Just c -> 
                    c
    in
    { m
        | currWave =
            cw + 1  -- this wave becomes the currently selected wave for the model
        , waves =
            ps ++ [(template f newColor)]  -- using our template function generate the default Wave record for the new wave
        , allColors = 
            List.drop 1 cols  -- since the first available color from the list has been used, drop it from the list
        , usedColors = 
            usedCols ++ [newColor]  -- add the new color to the list of currently used colors
    }


-- remove a wave from the model based on the provided index
removeWave idx m =
    let
        ps = m.waves

        len = List.length ps

        cw = m.currWave

        cols = m.allColors

        usedCols = m.usedColors

        currCol = List.take 1 (List.drop (idx - 1) usedCols)  -- using the provided index, figure out the waves color
    in
    { m
        | currWave =
            -- below conditionals are the logic for choosing which wave becomes the currently selected wave in the model once this wave is removed
            ( if cw == 1 then  
                1

            else if cw >= idx then
                cw - 1

            else
                cw
            )
        , waves =
            List.take (idx - 1) ps ++ List.drop idx ps  -- using the provided index, remove that wave from the list of models waves
        , allColors =
            cols ++ currCol  -- add the removed waves color back to list of available colors
        , usedColors = 
            List.take (idx - 1) usedCols ++ List.drop idx usedCols  -- remove the removed waves color from list of used colors
    }


-- helper function to create a string (from a float value) with a fixed number of elements 
showDigits width x =
    "      " ++ (String.fromFloat x |> String.left width) |> String.right width


-- convert the models amplitude to the value displayed to the user
amplitudeFinal v =
    v / 10


-- convert the models period to the value displayed to the user
periodFinal v =
    v / 10


-- convert the models phase shift to the value displayed to the user
phaseShiftFinal v =
    (v / 8) * 2


-- convert the models vertical shift to the value displayed to the user
vertShiftFinal v =
    v / 10


-- make the shift look pretty and fix its length
fixedPhaseText val =
    (if val < 0 then "-" else "+") ++ showDigits 4 (abs val)


-- make the amplitude look pretty and fix its length
fixedMultText width val =
    ("      " ++ (if val < 0 then "-" else " ")) ++ (String.fromFloat (abs val) |> String.left (width)) |> String.right (width + 1)


-- wave attributes to its equation
-- takes the model, a waves index and the wave itself
-- and converts all its Parameters and attributes into one equation represented as a Shape
functionText m idx wav =
    let
        equation =
            text (
                fixedMultText 4 (amplitudeFinal wav.uScale) ++ "\u{22C5}" ++ textTrig wav.trigCycleU  -- Amplitude text
                    ++ "(" ++ showDigits 4 (periodFinal wav.uDilation) ++ "\u{22C5}time "   -- Period text
                    ++ fixedPhaseText (phaseShiftFinal wav.uPhaseShift) ++ "\u{22C5}\u{1D70B}) "   -- Phase shift text
                    ++ fixedPhaseText (vertShiftFinal wav.uVertShift)  -- Vertical shift text
                --    ++ "  <-- " ++ (textTrig wav.trigCycleU) ++ "  " ++ (String.fromInt idx)  ++ "  " ++ (String.fromInt m.currWave) --++ "  " ++ (String.fromFloat m.currAmp) ++ "  " ++ (String.fromFloat m.currPer) ++ "  " ++ (String.fromFloat m.currPha) ++ "  " ++ (String.fromFloat m.currVer)
                -- above line is additional temp info
                ) |> fixedwidth |> size 10

        -- transparent rectangle, which when clicked, notifies us 
        -- to update the model so that this wave becomes the currently selected wave
        selectRect = 
            rect 206 12 |> filled (rgba 0 0 0 0) |> move (99, 3)
                |> notifyTap (TransM (\model -> { model | currWave = (idx + 1) }))           
        
        -- if this wave is the currently selected wave, make it nice and bold and add an rectangle outline around it
        condEquation = 
            if m.currWave == (idx + 1) then
                [ equation |> bold |> filled wav.uColor
                , selectRect |> addOutline (solid 1) wav.uColor ]
            
            else
                [ equation |> filled wav.uColor
                , selectRect ]
    in
    group condEquation |> move (35, -(20 * (Basics.toFloat (idx + 1))+3))  -- using the index, determines the equations location in the Equations section


-- function to create slider for the provided Parameter type
sliderShape model (x, y) param =
    let
        cw = model.currWave

        -- current value of the slider
        currVal  = 
            case (List.head (List.drop (cw - 1) model.waves)) of
                Nothing -> 
                    50  -- if there is no wave, defaults to the middle of the slider

                -- if there is a wave then
                -- for whichever Parameter this slider represents, generate its current value using modelToSlider
                -- this is because the slider expects values between 0 and 100 but the models keeps the values
                -- in a different way so that the transformations and adjustments can be applied easily
                Just w -> 
                    ( case param of
                        Amplitude ->
                            modelToSlider w.uScale param

                        Period ->
                            modelToSlider w.uDilation param

                        PhaseShift ->
                            modelToSlider w.uPhaseShift param

                        VerticalShift ->
                            modelToSlider w.uVertShift param  
                    )                              
        
        -- creates the Shape for the slider using its current value, which Parameter it is for, and the x, y coordinates it should be located at
        mainSlider = 
            group [ rect 100 7 |> filled (rgba 0 0 0 0)
                , rect 100 3.5 |> filled lightGrey |> addOutline (solid 0.5) darkGrey
                , circle 3 |> filled darkBlue |> addOutline (solid 0.5) black |> makeTransparent 1 |> move (-50 + currVal, 0)
                , text (textParam param) |> alignRight |> size 10 |> filled black |> move (-60, -3)
                ] |> move (x, y)
    in
    group [
        ( case cw of
            0 ->  -- if there are no waves, do not give notifications of mouse interactions, essentially disabling the slider
                mainSlider  

            _ -> -- otherwise (meaning if there are waves in the model)
                mainSlider
                    -- clicking on the slider starts the adjustment
                    |> notifyMouseDownAt (StartSlider param (x,y))
                    -- moving over the slider sends a message, but the update only accepts it if we are editing
                    |> notifyMouseMoveAt (Slider param (x,y))
                    -- both lifting the mouse, and mousing outside the slider stops the adjustment
                    |> notifyMouseUp EndSlider
                    |> notifyLeave EndSlider
        )
        -- current value of the slider displayed under the circle of the slider
        , text (sliderText (sliderToModel currVal param) param) |> centered |> fixedwidth |> size 7 |> bold
            |> filled black
            |> move (-50 + x + 1 * currVal, -11 + y)
        ]


-- Takes a float value and the Parameter type
-- and generates the text for the value so it can be shown under the respective slider
sliderText v param =
    let
        sign = (if v < 0 then "-" else "")

        cutter width x =
            String.fromFloat x |> String.left width
    in
        case param of
            Amplitude ->
                sign ++ cutter 4 (amplitudeFinal (abs v))

            Period ->
                cutter 4 (periodFinal v)

            PhaseShift ->
                sign ++ cutter 4 (phaseShiftFinal (abs v)) ++ "\u{22C5}\u{1D70B}"

            VerticalShift ->
                sign ++ cutter 4 (vertShiftFinal (abs v))


-- aggregation of all 4 sliders so that they can be moved together as one Shape given just one x, y coordinate
setOfSlider model (x,y) yDist =
    group 
        [ sliderShape model (x,y) Amplitude
        , sliderShape model (x,y-1*yDist) Period
        , sliderShape model (x,y-2*yDist) PhaseShift
        , sliderShape model (x,y-3*yDist) VerticalShift
        ]


-- converts the values 0-100 of the slider to values the model can use for transformations
sliderToModel newVal param =
    case param of
        Amplitude ->
            (newVal / (10/3)) - 5

        Period -> 
            newVal / 5

        PhaseShift ->
            (newVal / 12.5) - 4

        VerticalShift ->
            (newVal / 2.5) - 20


-- converts values the model uses to values that the slider expects internally
modelToSlider newVal param = 
    case param of
        Amplitude ->
            (newVal + 5) * (10/3)

        Period ->
            5 * newVal

        PhaseShift ->
            (newVal + 4) * 12.5

        VerticalShift ->
            (newVal + 20) * 2.5


-- Given a wave, a new value and a Parameter type
-- this function updates that Parameter in the waves record to equal the new value
waveParameters wave newVal param =
    let
        -- since the value comes from the slider we convert it a number the model expects
        convertedVal = sliderToModel newVal param  
    in
    case param of
        Amplitude ->
            { wave | uScale = convertedVal }

        Period ->
            { wave | uDilation = convertedVal }

        PhaseShift ->
            { wave | uPhaseShift = convertedVal, uArg = (wave.uArg + (convertedVal * Basics.pi) / 4) }

        VerticalShift ->
            { wave | uVertShift = convertedVal }


-- helper function that uses simple math functions like remainderBy to check whether a value needs to be a fraction
-- if it does, it will write the value as an improper fraction (but will be missing the denominator)
-- otherwise, it will simply write the value as an integer
-- in both scenarios this function also adds a pi symbol using the unicode value of a pi symbol
gridToPiString v =
    let
        vNoPi = 
            (Basics.toFloat v) * (1 / 2)
    in
    if (remainderBy 2 v) == 0 then text (String.fromFloat vNoPi ++ "\u{1D70B}") else text (String.fromFloat (vNoPi * 2) ++ "\u{1D70B}") |> underline
