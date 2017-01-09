-- -- Touch anywhere on the screen to apply thrust to the white space capsule.


module Game exposing (..)

import Html exposing (Html, text)
import Html.Attributes exposing (..)
import AnimationFrame
import Mouse exposing (..)
import Window
import Time exposing (Time)
import Collage exposing (..)
import Text exposing (fromString)
import Element exposing (centered)
import List
import Color exposing (..)
import Task


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { height : Int
    , width : Int
    , mouseDown : Bool
    , mouseX : Int
    , mouseY : Int
    , objects : List (PhysicalObject {})
    }


type alias Point a =
    { a
        | x : Float
        , y : Float
    }


type alias PhysicalObject a =
    { a
        | pos : Point {}
        , vel : Point {}
        , acc : Point {}
        , mass : Float
        , thrustable : Bool
        , form : Form
    }


type alias Ship a =
    PhysicalObject a


model : Model
model =
    { height = 0
    , width = 0
    , mouseX = 0
    , mouseY = 0
    , mouseDown = False
    , objects =
        [ ship
        , sun
        , earth
        , comet1
        , comet2
        , comet3
        , comet4
        ]
    }


init : ( Model, Cmd Msg )
init =
    ( model, initialSizeCmd )


initialSizeCmd : Cmd Msg
initialSizeCmd =
    Task.perform (\size -> Resize size.width size.height) Window.size



-- UPDATE


type Msg
    = TimeUpdate Time
    | Resize Int Int
    | MouseDownPosition Int Int
    | MouseUpPosition Int Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        TimeUpdate dt ->
            ( nextUniverse model, Cmd.none )

        Resize w h ->
            ( { model | height = h, width = w }, Cmd.none )

        MouseDownPosition x y ->
            ( { model | mouseDown = True, mouseX = x, mouseY = y }, Cmd.none )

        MouseUpPosition x y ->
            ( { model | mouseDown = False }, Cmd.none )


nextUniverse : Model -> Model
nextUniverse model =
    let
        ship =
            List.head model.objects
    in
        case ship of
            Just ship_ ->
                { model | objects = List.map (moveObject timeDelta ( model.width, model.height ) ( model.mouseX, model.mouseY ) model.mouseDown ship_ model.objects) model.objects }

            Nothing ->
                model



-- VIEW


view : Model -> Html msg
view model =
    Html.div [ style [ ( "backgroundColor", "black" ) ] ] [ Element.toHtml <| renderUniverse model ]


renderUniverse : Model -> Element.Element
renderUniverse model =
    let
        ship =
            List.head model.objects
    in
        collage model.width
            model.height
            ([ toForm (Element.tiledImage model.width model.height "./images/stars.png")
             , velocityVectorForm ship
             , accelerationVectorForm ship
             ]
                ++ (universeForms model)
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ AnimationFrame.diffs TimeUpdate
        , Window.resizes (\{ width, height } -> Resize width height)
        , Mouse.downs (\{ x, y } -> MouseDownPosition x y)
        , Mouse.ups (\{ x, y } -> MouseUpPosition x y)
        ]



-- GAME LOGIC


epsilon : Float
epsilon =
    0.01



-- Minimal distance to still apply gravitational force.


timeDelta : Float
timeDelta =
    8000


inputFPS : Float
inputFPS =
    60


uranusSemiMajorAxis : Float
uranusSemiMajorAxis =
    2.8706714e12



-- meters


solarSystemDiameter : Float
solarSystemDiameter =
    uranusSemiMajorAxis * 2



-- meters


metersPerPixel : Float
metersPerPixel =
    solarSystemDiameter / 5000



-- meters / pixel


mercuryCapsuleMass : Float
mercuryCapsuleMass =
    2000



-- kg


cometMass : Float
cometMass =
    2.2e14



-- Halley's comet's mass is 2.2e14 kg.


rocketAcc : Float
rocketAcc =
    0.003755



-- m / s^2


gravitationalConstant : Float
gravitationalConstant =
    6.67384 * 1.0e-10



-- m^3 / kg s^2


earthMass : Float
earthMass =
    5.97219 * 1.0e25


earthOrbitalVelocity : Float
earthOrbitalVelocity =
    300000


sunMass : Float
sunMass =
    1.98855 * 1.0e31


sunOrbitalVelocity : Float
sunOrbitalVelocity =
    0



-- An Astronomical Unit (AU) is about the distance from the Sun to the Earth.


au : Float
au =
    149597870700



-- meters


rocketThrust : Float
rocketThrust =
    mercuryCapsuleMass * rocketAcc



-- F = ma : (Newtons)


metersToPixels : Float -> Float
metersToPixels meters =
    meters / metersPerPixel


formGen : Point {} -> Color -> Float -> String -> Form
formGen pos color radius name =
    move ( pos.x / metersPerPixel, pos.y / metersPerPixel )
        (group
            [ filled color (circle radius)
            , toForm (Element.centered <| Text.fromString name)
            ]
        )


generateBody :
    Point {}
    -> Point {}
    -> Float
    -> Color
    -> Float
    -> String
    -> Bool
    -> PhysicalObject {}
generateBody pos vel mass color radius name thrustable =
    { pos = pos
    , vel = vel
    , acc = { x = 0, y = 0 }
    , mass = mass
    , thrustable = thrustable
    , form = formGen pos color radius name
    }


comet : Point {} -> Point {} -> PhysicalObject {}
comet pos vel =
    generateBody pos vel cometMass lightRed 4 "" False



-- -- Physical Objects --


sun : PhysicalObject {}
sun =
    generateBody { x = 0, y = 0 }
        { x = 0, y = sunOrbitalVelocity }
        sunMass
        lightYellow
        30
        "☉"
        False


earth : PhysicalObject {}
earth =
    generateBody { x = sun.pos.x + au, y = sun.pos.y }
        { x = 0, y = earthOrbitalVelocity }
        earthMass
        lightBlue
        10
        "⊕"
        False


ship : PhysicalObject {}
ship =
    generateBody { x = earth.pos.x + 200000000, y = earth.pos.y }
        { x = 0, y = earthOrbitalVelocity + 10000 }
        mercuryCapsuleMass
        white
        3
        ""
        True


comet1 : PhysicalObject {}
comet1 =
    comet { x = au, y = au } { x = -200000, y = 80000 }


comet2 : PhysicalObject {}
comet2 =
    comet { x = au, y = -au } { x = -230000, y = -20000 }


comet3 : PhysicalObject {}
comet3 =
    comet { x = -au, y = au } { x = 250000, y = 150000 }


comet4 : PhysicalObject {}
comet4 =
    comet { x = au, y = -2 * au } { x = -200000, y = 80000 }



-- -- Physics Code --


distance : PhysicalObject a -> PhysicalObject a -> Float
distance a b =
    sqrt ((a.pos.x - b.pos.x) ^ 2 + (a.pos.y - b.pos.y) ^ 2)


universeForms : Model -> List Form
universeForms u =
    List.map (\o -> o.form) u.objects



-- force exhibited by B onto A


force : PhysicalObject a -> PhysicalObject a -> ( Float, Float )
force a b =
    let
        dist =
            distance a b

        unitX =
            (a.pos.x - b.pos.x) / dist

        unitY =
            (a.pos.y - b.pos.y) / dist

        -- F = GM1M2 / r^2
        f =
            -(gravitationalConstant * a.mass * b.mass / dist ^ 2)

        ( forceX, forceY ) =
            ( f * unitX, f * unitY )
    in
        if dist < epsilon then
            ( 0, 0 )
        else
            ( forceX, forceY )



-- generate thrust (force in newtons) from the rocket


thrust : Ship a -> ( Int, Int ) -> ( Int, Int ) -> Bool -> ( Float, Float )
thrust ship ( w_, h_ ) ( mx, my ) mouseDown =
    if mouseDown then
        let
            thrustVec =
                thrustVector ( w_, h_ ) ( mx, my ) ship

            ( vx, vy ) =
                ( thrustVec.x, thrustVec.y )
        in
            ( rocketThrust * vx, rocketThrust * vy )
    else
        ( 0, 0 )


tupleAdd : ( Float, Float ) -> ( Float, Float ) -> ( Float, Float )
tupleAdd ( a0, b0 ) ( a1, b1 ) =
    ( a0 + a1, b0 + b1 )



-- Move an object given the universe of physical objects.


moveObject :
    Float
    -> ( Int, Int )
    -> ( Int, Int )
    -> Bool
    -> Ship a
    -> List (PhysicalObject {})
    -> PhysicalObject {}
    -> PhysicalObject {}
moveObject dt ( w_, h_ ) ( mx, my ) mdown ship allobjects object =
    let
        forces =
            List.map (force object) allobjects

        ( forceX, forceY ) =
            List.foldr tupleAdd ( 0, 0 ) forces

        ( thrustX, thrustY ) =
            if (mdown && object.thrustable) then
                (thrust ship ( w_, h_ ) ( mx, my ) mdown)
            else
                ( 0, 0 )

        ( totalForceX, totalForceY ) =
            ( forceX + thrustX, forceY + thrustY )

        -- F = ma -> a = F / m
        ( ax_, ay_ ) =
            ( totalForceX / object.mass, totalForceY / object.mass )

        ( vx_, vy_ ) =
            ( object.vel.x + ax_ * dt, object.vel.y + ay_ * dt )
    in
        { acc = { x = ax_, y = ay_ }
        , vel = { x = vx_, y = vy_ }
        , pos =
            { x = object.pos.x + vx_ * dt
            , y = object.pos.y + vy_ * dt
            }
        , mass = object.mass
        , thrustable = object.thrustable
        , form =
            move
                ( object.vel.x * dt / metersPerPixel
                , object.vel.y * dt / metersPerPixel
                )
                object.form
        }


vector : Color -> Float -> Point a -> Point a -> Form
vector color scale originPt offsetPt =
    traced (solid color) <|
        path
            [ ( metersToPixels originPt.x, metersToPixels originPt.y )
            , ( metersToPixels originPt.x + scale * metersToPixels offsetPt.x
              , metersToPixels originPt.y + scale * metersToPixels offsetPt.y
              )
            ]


velocityScaleFactor : Float
velocityScaleFactor =
    1.0e5


accelerationScaleFactor : Float
accelerationScaleFactor =
    1.0e11


velocityVectorForm : Maybe (Ship a) -> Form
velocityVectorForm ship =
    let
        --Temp solution to handle maybe
        empty =
            { x = 0, y = 0 }
    in
        case ship of
            Just ship_ ->
                vector red velocityScaleFactor ship_.pos ship_.vel

            Nothing ->
                vector red velocityScaleFactor empty empty


accelerationVectorForm : Maybe (Ship a) -> Form
accelerationVectorForm ship =
    let
        --Temp solution to handle maybe
        empty =
            { x = 0, y = 0 }
    in
        case ship of
            Just ship_ ->
                vector blue accelerationScaleFactor ship_.pos ship_.acc

            Nothing ->
                vector blue accelerationScaleFactor empty empty


thrustVector : ( Int, Int ) -> ( Int, Int ) -> Ship a -> Point {}
thrustVector ( w_, h_ ) ( mx, my ) ship =
    let
        ( sx, sy ) =
            ( metersToPixels ship.pos.x, metersToPixels ship.pos.y )

        ( vxc, vyc ) =
            relativeToCenter ( w_, h_ ) ( mx, my )

        -- vector to the center
        ( vxs, vys ) =
            ( vxc - sx, vyc - sy )

        -- vector to the ship
        vfinish =
            { x = vxs, y = vys }
    in
        vfinish


relativeToCenter : ( Int, Int ) -> ( Int, Int ) -> ( Float, Float )
relativeToCenter ( w_, h_ ) ( x, y ) =
    ( toFloat x - toFloat w_ / 2, -(toFloat y) + toFloat h_ / 2 )


emptyPosition : ( Int, Int )
emptyPosition =
    ( 0, 0 )
