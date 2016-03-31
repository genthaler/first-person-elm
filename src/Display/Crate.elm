module Display.Crate (crate) where

import Math.Vector2 exposing (Vec2)
import Math.Vector3 exposing (..)
import Math.Matrix4 exposing (..)
import WebGL exposing (..)

--import Model

-- Define the mesh for a crate

type alias Vertex =
    { position : Vec3
    , coord : Vec3
    }


crate : Drawable Vertex
crate =
  Triangle (List.concatMap rotatedFace [ (0,0), (90,0), (180,0), (270,0), (0,90), (0,-90) ])


rotatedFace : (Float,Float) -> List (Vertex, Vertex, Vertex)
rotatedFace (angleXZ,angleYZ) =
  let
    x = makeRotate (degrees angleXZ) j
    y = makeRotate (degrees angleYZ) i
    t = x `mul` y
    each f (a,b,c) = (f a, f b, f c)
  in
    List.map (each (\v -> {v | position = transform t v.position })) face


face : List (Vertex, Vertex, Vertex)
face =
  let
    topLeft     = Vertex (vec3 -1  1 1) (vec3 0 1 0)
    topRight    = Vertex (vec3  1  1 1) (vec3 1 1 0)
    bottomLeft  = Vertex (vec3 -1 -1 1) (vec3 0 0 0)
    bottomRight = Vertex (vec3  1 -1 1) (vec3 1 0 0)
  in
    [ (topLeft,topRight,bottomLeft)
    , (bottomLeft,topRight,bottomRight)
    ]


-- Shaders

vertexShader : Shader { position:Vec3, coord:Vec3 } { u | perspective:Mat4 } { vcoord:Vec2 }
vertexShader = [glsl|

attribute vec3 position;
attribute vec3 coord;
uniform mat4 perspective;
varying vec2 vcoord;

void main () {
  gl_Position = perspective * vec4(position, 1.0);
  vcoord = coord.xy;
}

|]


fragmentShader : Shader {} { u | crate:Texture } { vcoord:Vec2 }
fragmentShader = [glsl|

precision mediump float;
uniform sampler2D crate;
varying vec2 vcoord;

void main () {
  gl_FragColor = texture2D(crate, vcoord);
}

|]
