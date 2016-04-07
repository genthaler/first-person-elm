module View (view) where

import Http exposing (..)
import Math.Vector2 exposing (Vec2)
import Math.Vector3 exposing (..)
import Math.Matrix4 exposing (..)
import WebGL exposing (..)
import Html exposing (..)
import Graphics.Element exposing (..)
import Model
import View.World exposing (world)
import View.Crate exposing (crate)
import Color exposing (..)

view : (Int,Int) -> Model.Person -> Mat4
view (w,h) person =
    mul (makePerspective 45 (toFloat w / toFloat h) 0.01 100)
        (makeLookAt person.position (person.position `add` Model.direction person) j)

scene : (Int,Int) -> Bool -> Maybe Texture -> Model.Person -> Element
scene (w,h) isLocked texture person =
    layers [ color (rgb 135 206 235) (spacer w h)
           , webgl (w,h) (entities texture (view (w,h) person))
           , container w 140 (midLeftAt (absolute 40) (relative 0.5))
                 (if isLocked then exitMsg else enterMsg)
           ]

entities : Response -> Mat4 -> List Renderable
entities response view =
    let crates = case response.status of
                   Success texture ->
                       [ crate texture view
                       , crate texture (translate3  10 0  10 view)
                       , crate texture (translate3 -10 0 -10 view)
                       ]
                   _ -> []
    in
        ground view :: crates

enterMsg : Element
enterMsg = message "Click to go full screen and move your head with the mouse."

exitMsg : Element
exitMsg = message "Press <escape> to exit full screen."

message : String -> Element
message msg =
    show <|
    "This uses stuff that is only available in Chrome and Firefox!\n" ++
    "\nWASD keys to move, space bar to jump.\n\n" ++ msg
