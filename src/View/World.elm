module View.World (ground) where

import Math.Vector3 exposing (..)
import Math.Matrix4 exposing (..)
import WebGL exposing (..)
import Color exposing (toRgb, hsl)
type alias Vertex = { position:Vec3, color:Vec3 }

world : Mat4 -> Renderable
world view =
    render vertexShader fragmentShader groundMesh { view=view }


-- Help create colors as Vectors
color : Float -> Float -> Float -> Vec3
color hue saturation lightness =
    let c = toRgb (hsl hue saturation lightness)
    in  vec3 (toFloat c.red / 255) (toFloat c.green / 255) (toFloat c.blue / 255)


-- The mesh for the ground
groundMesh : Drawable Vertex
groundMesh =
  let green = color (degrees 110) 0.48

      topLeft     = Vertex (vec3 -20 -1  20) (green 0.7)
      topRight    = Vertex (vec3  20 -1  20) (green 0.4)
      bottomLeft  = Vertex (vec3 -20 -1 -20) (green 0.5)
      bottomRight = Vertex (vec3  20 -1 -20) (green 0.6)
  in
      Triangle [ (topLeft,topRight,bottomLeft), (bottomLeft,topRight,bottomRight) ]


-- Shaders
vertexShader : Shader Vertex { view:Mat4 } { vcolor:Vec3 }
vertexShader = [glsl|

attribute vec3 position;
attribute vec3 color;
uniform mat4 view;
varying vec3 vcolor;

void main () {
    gl_Position = view * vec4(position, 1.0);
    vcolor = color;
}

|]

fragmentShader : Shader {} u { vcolor:Vec3 }
fragmentShader = [glsl|

precision mediump float;
varying vec3 vcolor;

void main () {
    gl_FragColor = vec4(vcolor, 1.0);
}

|]
