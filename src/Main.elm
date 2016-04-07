module Main where

import Keyboard
import Mouse
import Window

import Model
import Update
import View

-- Pointer Lock information
port movement : Signal (Int,Int)
port isLocked : Signal Bool

-- Set up 3D world
world : Maybe Texture -> Mat4 -> List Renderable
world maybeTexture perspective =
  case maybeTexture of
    Nothing ->
        []

    Just tex ->
        [render vertexShader fragmentShader crate { crate=tex, perspective=perspective }]


main : Signal Element
main =
  let
    person =
      Signal.foldp update defaultPerson inputs

    entities =
      Signal.map2 world
        texture.signal
        (Signal.map2 perspective Window.dimensions person)
  in
    Signal.map2 view Window.dimensions entities


texture : Signal.Mailbox (Maybe Texture)
texture =
  Signal.mailbox Nothing


port fetchTexture : Task WebGL.Error ()
port fetchTexture =
  loadTexture "/resources/texture/woodCrate.jpg"
    `Task.andThen` \tex -> Signal.send texture.address (Just tex)


inputs : Signal Inputs
inputs =
  let
    dt = Signal.map (\t -> t/500) (fps 25)
  in
    Signal.map3 (,,) Keyboard.space Keyboard.arrows dt
      |> Signal.sampleOn dt

-- Ability to request and exit. Click screen to request lock. Press escape to
-- give up the lock. This code can all be removed if you want to do this
-- differently.

port requestPointerLock : Signal ()
port requestPointerLock =
    dropWhen (lift2 (&&) Keyboard.shift isLocked) () Mouse.clicks

port exitPointerLock : Signal ()
port exitPointerLock =
    always () <~ keepIf (any (\x -> x == 27)) [] Keyboard.keysDown
