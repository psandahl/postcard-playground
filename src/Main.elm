module Main exposing (main)

import Html exposing (Html)
import Html.Attributes as Attr
import List.Extra as List
import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector3 as Vec3 exposing (Vec3)
import WebGL as GL exposing (Mesh, Shader)
import WebGL.Settings as Settings
import WebGL.Settings.DepthTest as DepthTest


type alias Model =
    { perspective : Mat4
    , view : Mat4
    , flatTerrainMesh : Mesh Vertex
    }


type Msg
    = Nop


type alias Vertex =
    { position : Vec3
    }


init : ( Model, Cmd Msg )
init =
    ( { perspective = Mat4.makePerspective 45 (toFloat width / toFloat height) 0.1 500
      , view = Mat4.makeLookAt (Vec3.vec3 -30 20 -30) (Vec3.vec3 1 0 1) (Vec3.vec3 0 1 0)
      , flatTerrainMesh = makeFlatTerrainMesh
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Nop ->
            ( model, Cmd.none )


view : Model -> Html Msg
view model =
    GL.toHtmlWith
        [ GL.antialias
        , GL.depth 1
        , GL.alpha False
        , GL.clearColor 0 0 (45 / 255) 1
        ]
        [ Attr.height height
        , Attr.width width
        ]
        [ GL.entityWith
            [ DepthTest.default
            , Settings.cullFace Settings.back
            ]
            terrainVertexShader
            terrainFragmentShader
            model.flatTerrainMesh
            { perspective = model.perspective
            , view = model.view
            }
        ]


width : Int
width =
    800


height : Int
height =
    600


makeFlatTerrainMesh : Mesh Vertex
makeFlatTerrainMesh =
    let
        vertices =
            terrainVertices 65 65

        indices =
            terrainIndices 65 65
    in
    GL.indexedTriangles vertices indices


terrainVertices : Int -> Int -> List Vertex
terrainVertices rows cols =
    -- Rows and cols are in vertices.
    List.initialize (rows * cols) <|
        \vertice ->
            let
                z =
                    vertice // cols

                x =
                    vertice % cols
            in
            { position = Vec3.vec3 (toFloat x) 0 (toFloat z) }


terrainIndices : Int -> Int -> List ( Int, Int, Int )
terrainIndices rows cols =
    -- Rows and cols are in vertices
    List.initialize ((rows - 1) * (cols - 1) * 2) <|
        \triangle ->
            let
                quad =
                    triangle // 2

                half =
                    triangle % 2

                row =
                    quad // (rows - 1)

                col =
                    quad % (cols - 1)

                v0 =
                    col + (row * cols)

                v1 =
                    v0 + 1

                v2 =
                    col + ((row + 1) * cols)

                v3 =
                    v2 + 1
            in
            if isEven half then
                ( v1, v0, v2 )
            else
                ( v1, v2, v3 )


isEven : Int -> Bool
isEven n =
    n % 2 == 0


terrainVertexShader :
    Shader Vertex
        { uniforms
            | perspective : Mat4
            , view : Mat4
        }
        { vColor : Vec3 }
terrainVertexShader =
    [glsl|
precision mediump float;

attribute vec3 position;

uniform mat4 perspective;
uniform mat4 view;

varying vec3 vColor;

vec3 ambientLightColor = vec3(1.0);
float ambientLightStrength = 0.2;

//vec3 sunLightColor = vec3(182.0 / 255.0, 126.0 / 255.0, 91.0 / 255.0);
vec3 sunLightColor = vec3(1.0);
vec3 sunDirection = normalize(vec3(1.0, 1.0, 0.0));

vec3 ambientLight();
vec3 sunLight(vec3 normal);

vec3 adjustHeight(vec3 pos);

vec3 mod289(vec3 x);
vec2 mod289(vec2 x);
vec3 permute(vec3 x);
float snoise(vec2 v);

void main()
{
    vec3 normal = vec3(0.0, 1.0, 0.0);

    vColor = vec3(0.3) * (ambientLight() + sunLight(normal));

    mat4 mvp = perspective * view;
    gl_Position = mvp * vec4(adjustHeight(position), 1.0);
}

vec3 ambientLight()
{
    return ambientLightColor * ambientLightStrength;
}

vec3 sunLight(vec3 normal)
{
    float diffuse = max(dot(normal, sunDirection), 0.0);
    return sunLightColor * diffuse;
}

vec3 adjustHeight(vec3 pos)
{
    float dividend = 65.0;
    vec2 inp = vec2(pos.x / dividend, pos.z / dividend);
    float h = snoise(inp) * 5.0;

    return vec3(pos.x, h, pos.z);
}

//
// Description : Array and textureless GLSL 2D simplex noise function.
//      Author : Ian McEwan, Ashima Arts.
//  Maintainer : stegu
//     Lastmod : 20110822 (ijm)
//     License : Copyright (C) 2011 Ashima Arts. All rights reserved.
//               Distributed under the MIT License. See LICENSE file.
//               https://github.com/ashima/webgl-noise
//               https://github.com/stegu/webgl-noise
//

vec3 mod289(vec3 x) {
  return x - floor(x * (1.0 / 289.0)) * 289.0;
}

vec2 mod289(vec2 x) {
  return x - floor(x * (1.0 / 289.0)) * 289.0;
}

vec3 permute(vec3 x) {
  return mod289(((x*34.0)+1.0)*x);
}

float snoise(vec2 v)
{
  const vec4 C = vec4(0.211324865405187,  // (3.0-sqrt(3.0))/6.0
                      0.366025403784439,  // 0.5*(sqrt(3.0)-1.0)
                     -0.577350269189626,  // -1.0 + 2.0 * C.x
                      0.024390243902439); // 1.0 / 41.0
// First corner
  vec2 i  = floor(v + dot(v, C.yy) );
  vec2 x0 = v -   i + dot(i, C.xx);

// Other corners
  vec2 i1;
  //i1.x = step( x0.y, x0.x ); // x0.x > x0.y ? 1.0 : 0.0
  //i1.y = 1.0 - i1.x;
  i1 = (x0.x > x0.y) ? vec2(1.0, 0.0) : vec2(0.0, 1.0);
  // x0 = x0 - 0.0 + 0.0 * C.xx ;
  // x1 = x0 - i1 + 1.0 * C.xx ;
  // x2 = x0 - 1.0 + 2.0 * C.xx ;
  vec4 x12 = x0.xyxy + C.xxzz;
  x12.xy -= i1;

// Permutations
  i = mod289(i); // Avoid truncation effects in permutation
  vec3 p = permute( permute( i.y + vec3(0.0, i1.y, 1.0 ))
		+ i.x + vec3(0.0, i1.x, 1.0 ));

  vec3 m = max(0.5 - vec3(dot(x0,x0), dot(x12.xy,x12.xy), dot(x12.zw,x12.zw)), 0.0);
  m = m*m ;
  m = m*m ;

// Gradients: 41 points uniformly over a line, mapped onto a diamond.
// The ring size 17*17 = 289 is close to a multiple of 41 (41*7 = 287)

  vec3 x = 2.0 * fract(p * C.www) - 1.0;
  vec3 h = abs(x) - 0.5;
  vec3 ox = floor(x + 0.5);
  vec3 a0 = x - ox;

// Normalise gradients implicitly by scaling m
// Approximation of: m *= inversesqrt( a0*a0 + h*h );
  m *= 1.79284291400159 - 0.85373472095314 * ( a0*a0 + h*h );

// Compute final noise value at P
  vec3 g;
  g.x  = a0.x  * x0.x  + h.x  * x0.y;
  g.yz = a0.yz * x12.xz + h.yz * x12.yw;
  return 130.0 * dot(m, g);
}

    |]


terrainFragmentShader : Shader {} uniforms { vColor : Vec3 }
terrainFragmentShader =
    [glsl|
precision mediump float;

varying vec3 vColor;

void main()
{
    gl_FragColor = vec4(vColor, 1.0);
}

    |]


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        }
