-- Click anywhere on the screen to apply thrust to the white space capsule.

import Window
import Mouse

epsilon = 0.01 -- Minimal distance to still apply gravitational force.
timeDelta = 8000
inputFPS = 60
uranusSemiMajorAxis = 2.8706714e+12 -- meters
solarSystemDiameter = uranusSemiMajorAxis * 2 -- meters
metersPerPixel = solarSystemDiameter / 5000 -- meters / pixel
mercuryCapsuleMass = 2000 -- kg
cometMass = 2.2e14 -- Halley's comet's mass is 2.2e14 kg.
rocketAcc = 0.003755 -- m / s^2
gravitationalConstant = 6.67384 * 10e-11 -- m^3 / kg s^2
earthMass = 5.97219 * 10e24
earthOrbitalVelocity = 300000
sunMass = 1.98855 * 10e30
sunOrbitalVelocity = 0
-- An Astronomical Unit (AU) is about the distance from the Sun to the Earth.
au = 149597870700 -- meters

rocketThrust : Float
rocketThrust = mercuryCapsuleMass * rocketAcc -- F = ma : (Newtons)


type Point a =
    { a | x : Float
        , y : Float
        }

type PhysicalObject a =
    { a | pos : Point {}
        , vel : Point {}
        , acc : Point {}
        , mass : Float
        , thrustable : Bool
        , form : Form
        }

type Ship a = PhysicalObject a

type Universe =
    { objects : [ PhysicalObject {} ]
    }

metersToPixels meters =
    meters / metersPerPixel

formGen : Point {} -> Color -> Float -> String -> Form
formGen pos color radius name =
    move (pos.x / metersPerPixel, pos.y / metersPerPixel)
         (group [ filled color (circle radius)
                , toForm (plainText name) ])

generateBody : Point {} -> Point {} -> Float -> Color -> Float -> String
            -> Bool -> PhysicalObject {}
generateBody pos vel mass color radius name thrustable =
    { pos=pos
    , vel=vel
    , acc={ x=0, y=0 }
    , mass=mass
    , thrustable=thrustable
    , form=formGen pos color radius name
    }

comet : Point {} -> Point {} -> PhysicalObject {}
comet pos vel =
    generateBody pos vel cometMass lightRed 4 "" False

-- Physical Objects --

sun = generateBody { x=0, y=0 } { x=0, y=sunOrbitalVelocity }
                   sunMass lightYellow 30 "☉" False

earth = generateBody { x=sun.pos.x + au, y=sun.pos.y }
                     { x=0, y=earthOrbitalVelocity }
                     earthMass lightBlue 10 "⊕" False

ship = generateBody { x=earth.pos.x + 200000000, y=earth.pos.y }
                    { x=0, y=earthOrbitalVelocity + 10000 }
                    mercuryCapsuleMass white 3 "" True

comet1 = comet { x=au, y=au } { x=-200000, y=80000 }
comet2 = comet { x=au, y=-au } { x=-230000, y=-20000 }
comet3 = comet { x=-au, y=au } { x=250000, y=150000 }
comet4 = comet { x=au, y=-2 * au } { x=-200000, y=80000 }

theUniverse : Universe
theUniverse = { objects=[ ship, sun, earth
                        , comet1, comet2, comet3, comet4 ] }

-- Physics Code --

distance : PhysicalObject a -> PhysicalObject a -> Float
distance a b =
    sqrt ((a.pos.x - b.pos.x)^2 + (a.pos.y - b.pos.y)^2)

universeForms : Universe -> [Form]
universeForms u =
    map (\o -> o.form) u.objects

-- force exhibited by B onto A
force : PhysicalObject a -> PhysicalObject a -> (Float, Float)
force a b =
    let
        dist = distance a b
        unitX = (a.pos.x - b.pos.x) / dist
        unitY = (a.pos.y - b.pos.y) / dist
        -- F = GM1M2 / r^2
        f = -(gravitationalConstant * a.mass * b.mass / dist^2)
        (forceX, forceY) = (f * unitX, f * unitY)
    in if dist < epsilon
       then (0, 0)
       else (forceX, forceY)

-- generate thrust (force in newtons) from the rocket
thrust : Ship a -> (Int, Int) -> (Int, Int) -> Bool -> (Float, Float)
thrust ship (w', h') (mx, my) mouseDown =
    if mouseDown
    then let thrustVec = thrustVector (w', h') (mx, my) ship
             (vx, vy) = (thrustVec.x, thrustVec.y)
          in (rocketThrust * vx, rocketThrust * vy)
    else (0, 0)

tupleAdd : (Float, Float) -> (Float, Float) -> (Float, Float)
tupleAdd (a0, b0) (a1, b1) =
    (a0 + a1, b0 + b1)

-- Move an object given the universe of physical objects.
moveObject : Float -> (Int, Int) -> (Int, Int) -> Bool -> Ship a
          -> [PhysicalObject {}] -> PhysicalObject {} -> PhysicalObject {}
moveObject dt (w', h') (mx, my) mdown ship allobjects object =
    let forces = map (force object) allobjects
        (forceX, forceY) = foldr tupleAdd (0, 0) forces
        (thrustX, thrustY) = if (mdown && object.thrustable)
                             then (thrust ship (w', h') (mx, my) mdown)
                             else (0, 0)
        (totalForceX, totalForceY) = (forceX + thrustX, forceY + thrustY)
        -- F = ma -> a = F / m
        (ax', ay') = (totalForceX / object.mass, totalForceY / object.mass)
    in { acc = { x = ax', y = ay'}
       , vel = { x = object.vel.x + ax' * dt
               , y = object.vel.y + ay' * dt }
       , pos = { x = object.pos.x + object.vel.x * dt
               , y = object.pos.y + object.vel.y * dt }
       , mass = object.mass
       , thrustable = object.thrustable
       , form = move ( object.vel.x * dt / metersPerPixel
                     , object.vel.y * dt / metersPerPixel ) object.form }

nextUniverse : ((Int, Int), (Int, Int), Bool) -> Universe -> Universe
nextUniverse ((w', h'), mpos, mdown) universe =
    let ship = head universe.objects
        mvFn = moveObject timeDelta (w', h') mpos mdown ship universe.objects
    in { universe | objects <- map mvFn universe.objects }

vector : Color -> Float -> Point a -> Point a -> Form
vector color scale originPt offsetPt =
    traced (solid color)
      [ ( metersToPixels originPt.x, metersToPixels originPt.y )
      , ( metersToPixels originPt.x + scale * metersToPixels offsetPt.x
        , metersToPixels originPt.y + scale * metersToPixels offsetPt.y ) ]

velocityScaleFactor = 1e5
accelerationScaleFactor= 1e11

velocityVectorForm : Ship a -> Form
velocityVectorForm ship =
    vector red velocityScaleFactor ship.pos ship.vel

accelerationVectorForm : Ship a -> Form
accelerationVectorForm ship =
    vector blue accelerationScaleFactor ship.pos ship.acc

thrustVector : (Int, Int) -> (Int, Int) -> Ship a -> Point {}
thrustVector (w', h') (mx, my) ship =
    let (sx, sy) = (metersToPixels ship.pos.x, metersToPixels ship.pos.y)
        (vxc, vyc) = relativeToCenter (w', h') (mx, my) -- vector to the center
        (vxs, vys) = (vxc - sx, vyc - sy)  -- vector to the ship
        vfinish = { x = vxs, y = vys }
    in vfinish

relativeToCenter : (Int, Int) -> (Int, Int) -> (Float, Float)
relativeToCenter (w', h') (x, y) =
    ( toFloat x - toFloat w' / 2, -(toFloat y) + toFloat h' / 2 )

renderUniverse : (Int, Int) -> Universe -> Element
renderUniverse (w', h') universe =
    let ship = head universe.objects
        forms =  velocityVectorForm ship :: accelerationVectorForm ship
                                         :: universeForms universe
    in  collage w' h' ([ toForm (tiledImage w' h' "images/stars.png")
                       , velocityVectorForm ship
                       , accelerationVectorForm ship
                       ] ++ (universeForms universe))

input : Signal ((Int, Int), (Int, Int), Bool)
input = sampleOn (fps inputFPS)
                 ((,,) <~ Window.dimensions
                        ~ Mouse.position
                        ~ Mouse.isDown)

main = renderUniverse <~ Window.dimensions
                       ~ (foldp nextUniverse theUniverse input)
