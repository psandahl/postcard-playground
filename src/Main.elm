module Main exposing (main)

import Array
import Debug
import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events as Events
import Json.Decode as Decode
import List.Extra as List
import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector2 as Vec2 exposing (Vec2)
import Math.Vector3 as Vec3 exposing (Vec3)
import Mouse exposing (Position)
import WebGL as GL exposing (Mesh, Shader)
import WebGL.Settings as Settings
import WebGL.Settings.DepthTest as DepthTest


type alias Model =
    { perspective : Mat4
    , view : Mat4
    , tileMatrices : List Mat4
    , worldOffset : Vec2
    , flatTerrainMesh : Mesh Vertex
    , dragPosition : Maybe Position
    }


type Msg
    = DragStart Position
    | DragAt Position
    | DragEnd Position


type alias Vertex =
    { aPosition : Vec3
    }


init : ( Model, Cmd Msg )
init =
    ( { perspective = Mat4.makePerspective 45 (toFloat width / toFloat height) 1.0 1100
      , view = Mat4.makeLookAt (Vec3.vec3 0 250 0) (Vec3.vec3 0 5 -600) (Vec3.vec3 0 1 0)
      , tileMatrices = tileMatrices
      , worldOffset = Vec2.vec2 0 0
      , flatTerrainMesh = makeFlatTerrainMesh
      , dragPosition = Nothing
      }
    , Cmd.none
    )


tileMatrices : List Mat4
tileMatrices =
    [ -- Closest row
      Mat4.makeTranslate3 -(tileSizeFloat - 1) 0 -tileSizeFloat
    , Mat4.makeTranslate3 0 0 -tileSizeFloat

    -- + 1
    , Mat4.makeTranslate3 -((tileSizeFloat - 1) * 2) 0 -((tileSizeFloat - 1) * 2)
    , Mat4.makeTranslate3 -((tileSizeFloat - 1) * 1) 0 -((tileSizeFloat - 1) * 2)
    , Mat4.makeTranslate3 0 0 -((tileSizeFloat - 1) * 2)
    , Mat4.makeTranslate3 ((tileSizeFloat - 1) * 1) 0 -((tileSizeFloat - 1) * 2)

    -- + 2
    , Mat4.makeTranslate3 -((tileSizeFloat - 1) * 2) 0 -((tileSizeFloat - 1) * 3)
    , Mat4.makeTranslate3 -((tileSizeFloat - 1) * 1) 0 -((tileSizeFloat - 1) * 3)
    , Mat4.makeTranslate3 0 0 -((tileSizeFloat - 1) * 3)
    , Mat4.makeTranslate3 ((tileSizeFloat - 1) * 1) 0 -((tileSizeFloat - 1) * 3)

    -- + 3
    , Mat4.makeTranslate3 -((tileSizeFloat - 1) * 3) 0 -((tileSizeFloat - 1) * 4)
    , Mat4.makeTranslate3 -((tileSizeFloat - 1) * 2) 0 -((tileSizeFloat - 1) * 4)
    , Mat4.makeTranslate3 -((tileSizeFloat - 1) * 1) 0 -((tileSizeFloat - 1) * 4)
    , Mat4.makeTranslate3 0 0 -((tileSizeFloat - 1) * 4)
    , Mat4.makeTranslate3 ((tileSizeFloat - 1) * 1) 0 -((tileSizeFloat - 1) * 4)
    , Mat4.makeTranslate3 ((tileSizeFloat - 1) * 2) 0 -((tileSizeFloat - 1) * 4)

    -- + 4
    , Mat4.makeTranslate3 -((tileSizeFloat - 1) * 3) 0 -((tileSizeFloat - 1) * 5)
    , Mat4.makeTranslate3 -((tileSizeFloat - 1) * 2) 0 -((tileSizeFloat - 1) * 5)
    , Mat4.makeTranslate3 -((tileSizeFloat - 1) * 1) 0 -((tileSizeFloat - 1) * 5)
    , Mat4.makeTranslate3 0 0 -((tileSizeFloat - 1) * 5)
    , Mat4.makeTranslate3 ((tileSizeFloat - 1) * 1) 0 -((tileSizeFloat - 1) * 5)
    , Mat4.makeTranslate3 ((tileSizeFloat - 1) * 2) 0 -((tileSizeFloat - 1) * 5)

    -- + 5
    , Mat4.makeTranslate3 -((tileSizeFloat - 1) * 4) 0 -((tileSizeFloat - 1) * 6)
    , Mat4.makeTranslate3 -((tileSizeFloat - 1) * 3) 0 -((tileSizeFloat - 1) * 6)
    , Mat4.makeTranslate3 -((tileSizeFloat - 1) * 2) 0 -((tileSizeFloat - 1) * 6)
    , Mat4.makeTranslate3 -((tileSizeFloat - 1) * 1) 0 -((tileSizeFloat - 1) * 6)
    , Mat4.makeTranslate3 0 0 -((tileSizeFloat - 1) * 6)
    , Mat4.makeTranslate3 ((tileSizeFloat - 1) * 1) 0 -((tileSizeFloat - 1) * 6)
    , Mat4.makeTranslate3 ((tileSizeFloat - 1) * 2) 0 -((tileSizeFloat - 1) * 6)
    , Mat4.makeTranslate3 ((tileSizeFloat - 1) * 3) 0 -((tileSizeFloat - 1) * 6)

    -- + 6
    , Mat4.makeTranslate3 -((tileSizeFloat - 1) * 4) 0 -((tileSizeFloat - 1) * 7)
    , Mat4.makeTranslate3 -((tileSizeFloat - 1) * 3) 0 -((tileSizeFloat - 1) * 7)
    , Mat4.makeTranslate3 -((tileSizeFloat - 1) * 2) 0 -((tileSizeFloat - 1) * 7)
    , Mat4.makeTranslate3 -((tileSizeFloat - 1) * 1) 0 -((tileSizeFloat - 1) * 7)
    , Mat4.makeTranslate3 0 0 -((tileSizeFloat - 1) * 7)
    , Mat4.makeTranslate3 ((tileSizeFloat - 1) * 1) 0 -((tileSizeFloat - 1) * 7)
    , Mat4.makeTranslate3 ((tileSizeFloat - 1) * 2) 0 -((tileSizeFloat - 1) * 7)
    , Mat4.makeTranslate3 ((tileSizeFloat - 1) * 3) 0 -((tileSizeFloat - 1) * 7)

    -- + 7
    , Mat4.makeTranslate3 -((tileSizeFloat - 1) * 5) 0 -((tileSizeFloat - 1) * 8)
    , Mat4.makeTranslate3 -((tileSizeFloat - 1) * 4) 0 -((tileSizeFloat - 1) * 8)
    , Mat4.makeTranslate3 -((tileSizeFloat - 1) * 3) 0 -((tileSizeFloat - 1) * 8)
    , Mat4.makeTranslate3 -((tileSizeFloat - 1) * 2) 0 -((tileSizeFloat - 1) * 8)
    , Mat4.makeTranslate3 -((tileSizeFloat - 1) * 1) 0 -((tileSizeFloat - 1) * 8)
    , Mat4.makeTranslate3 0 0 -((tileSizeFloat - 1) * 8)
    , Mat4.makeTranslate3 ((tileSizeFloat - 1) * 1) 0 -((tileSizeFloat - 1) * 8)
    , Mat4.makeTranslate3 ((tileSizeFloat - 1) * 2) 0 -((tileSizeFloat - 1) * 8)
    , Mat4.makeTranslate3 ((tileSizeFloat - 1) * 3) 0 -((tileSizeFloat - 1) * 8)
    , Mat4.makeTranslate3 ((tileSizeFloat - 1) * 4) 0 -((tileSizeFloat - 1) * 8)

    -- + 8
    , Mat4.makeTranslate3 -((tileSizeFloat - 1) * 6) 0 -((tileSizeFloat - 1) * 9)
    , Mat4.makeTranslate3 -((tileSizeFloat - 1) * 5) 0 -((tileSizeFloat - 1) * 9)
    , Mat4.makeTranslate3 -((tileSizeFloat - 1) * 4) 0 -((tileSizeFloat - 1) * 9)
    , Mat4.makeTranslate3 -((tileSizeFloat - 1) * 3) 0 -((tileSizeFloat - 1) * 9)
    , Mat4.makeTranslate3 -((tileSizeFloat - 1) * 2) 0 -((tileSizeFloat - 1) * 9)
    , Mat4.makeTranslate3 -((tileSizeFloat - 1) * 1) 0 -((tileSizeFloat - 1) * 9)
    , Mat4.makeTranslate3 0 0 -((tileSizeFloat - 1) * 9)
    , Mat4.makeTranslate3 ((tileSizeFloat - 1) * 1) 0 -((tileSizeFloat - 1) * 9)
    , Mat4.makeTranslate3 ((tileSizeFloat - 1) * 2) 0 -((tileSizeFloat - 1) * 9)
    , Mat4.makeTranslate3 ((tileSizeFloat - 1) * 3) 0 -((tileSizeFloat - 1) * 9)
    , Mat4.makeTranslate3 ((tileSizeFloat - 1) * 4) 0 -((tileSizeFloat - 1) * 9)
    , Mat4.makeTranslate3 ((tileSizeFloat - 1) * 5) 0 -((tileSizeFloat - 1) * 9)

    -- + 9
    , Mat4.makeTranslate3 -((tileSizeFloat - 1) * 6) 0 -((tileSizeFloat - 1) * 10)
    , Mat4.makeTranslate3 -((tileSizeFloat - 1) * 5) 0 -((tileSizeFloat - 1) * 10)
    , Mat4.makeTranslate3 -((tileSizeFloat - 1) * 4) 0 -((tileSizeFloat - 1) * 10)
    , Mat4.makeTranslate3 -((tileSizeFloat - 1) * 3) 0 -((tileSizeFloat - 1) * 10)
    , Mat4.makeTranslate3 -((tileSizeFloat - 1) * 2) 0 -((tileSizeFloat - 1) * 10)
    , Mat4.makeTranslate3 -((tileSizeFloat - 1) * 1) 0 -((tileSizeFloat - 1) * 10)
    , Mat4.makeTranslate3 0 0 -((tileSizeFloat - 1) * 10)
    , Mat4.makeTranslate3 ((tileSizeFloat - 1) * 1) 0 -((tileSizeFloat - 1) * 10)
    , Mat4.makeTranslate3 ((tileSizeFloat - 1) * 2) 0 -((tileSizeFloat - 1) * 10)
    , Mat4.makeTranslate3 ((tileSizeFloat - 1) * 3) 0 -((tileSizeFloat - 1) * 10)
    , Mat4.makeTranslate3 ((tileSizeFloat - 1) * 4) 0 -((tileSizeFloat - 1) * 10)
    , Mat4.makeTranslate3 ((tileSizeFloat - 1) * 5) 0 -((tileSizeFloat - 1) * 10)

    -- + 10
    , Mat4.makeTranslate3 -((tileSizeFloat - 1) * 6) 0 -((tileSizeFloat - 1) * 11)
    , Mat4.makeTranslate3 -((tileSizeFloat - 1) * 5) 0 -((tileSizeFloat - 1) * 11)
    , Mat4.makeTranslate3 -((tileSizeFloat - 1) * 4) 0 -((tileSizeFloat - 1) * 11)
    , Mat4.makeTranslate3 -((tileSizeFloat - 1) * 3) 0 -((tileSizeFloat - 1) * 11)
    , Mat4.makeTranslate3 -((tileSizeFloat - 1) * 2) 0 -((tileSizeFloat - 1) * 11)
    , Mat4.makeTranslate3 -((tileSizeFloat - 1) * 1) 0 -((tileSizeFloat - 1) * 11)
    , Mat4.makeTranslate3 0 0 -((tileSizeFloat - 1) * 11)
    , Mat4.makeTranslate3 ((tileSizeFloat - 1) * 1) 0 -((tileSizeFloat - 1) * 11)
    , Mat4.makeTranslate3 ((tileSizeFloat - 1) * 2) 0 -((tileSizeFloat - 1) * 11)
    , Mat4.makeTranslate3 ((tileSizeFloat - 1) * 3) 0 -((tileSizeFloat - 1) * 11)
    , Mat4.makeTranslate3 ((tileSizeFloat - 1) * 4) 0 -((tileSizeFloat - 1) * 11)
    , Mat4.makeTranslate3 ((tileSizeFloat - 1) * 5) 0 -((tileSizeFloat - 1) * 11)
    ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        DragStart pos ->
            ( { model | dragPosition = Just pos }, Cmd.none )

        DragAt pos ->
            let
                ( deltaX, deltaY ) =
                    positionDeltas model.dragPosition pos

                deltaOffset =
                    Vec2.vec2 (toFloat deltaX) (toFloat deltaY)
            in
            ( { model
                | worldOffset = Vec2.add model.worldOffset deltaOffset
                , dragPosition = Just pos
              }
            , Cmd.none
            )

        DragEnd pos ->
            ( { model | dragPosition = Nothing }, Cmd.none )


positionDeltas : Maybe Position -> Position -> ( Int, Int )
positionDeltas mOldPos pos =
    case mOldPos of
        Just oldPos ->
            ( oldPos.x - pos.x, oldPos.y - pos.y )

        Nothing ->
            ( 0, 0 )


view : Model -> Html Msg
view model =
    Html.div
        []
        [ renderTerrain model
        , renderTools model
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.dragPosition of
        Nothing ->
            Sub.none

        Just _ ->
            Sub.batch [ Mouse.moves DragAt, Mouse.ups DragEnd ]


renderTools : Model -> Html Msg
renderTools model =
    Html.div
        []
        [ renderSlider "Base Wave Length" 1024 1 4096
        ]


renderSlider : String -> Int -> Int -> Int -> Html Msg
renderSlider caption value min_ max_ =
    Html.div []
        [ Html.text caption
        , Html.input
            [ Attr.type_ "range"
            , Attr.value (toString value)
            , Attr.min (toString min_)
            , Attr.max (toString max_)
            ]
            []
        ]


renderTerrain : Model -> Html Msg
renderTerrain model =
    Html.div
        [ onMouseDown
        , Attr.style
            [ ( "cursor"
              , case model.dragPosition of
                    Nothing ->
                        "default"

                    Just _ ->
                        "move"
              )
            ]
        ]
        [ GL.toHtmlWith
            [ GL.antialias
            , GL.depth 1
            , GL.alpha False
            , GL.clearColor 0 0 (45 / 255) 1
            ]
            [ Attr.height height
            , Attr.width width
            ]
          <|
            List.map
                (\tileMatrix ->
                    GL.entityWith
                        [ DepthTest.default
                        , Settings.cullFace Settings.back
                        ]
                        terrainVertexShader
                        terrainFragmentShader
                        model.flatTerrainMesh
                        { uPerspective = model.perspective
                        , uView = model.view
                        , uWorld = tileMatrix
                        , uWorldOffset = model.worldOffset
                        }
                )
                model.tileMatrices
        ]


onMouseDown : Html.Attribute Msg
onMouseDown =
    Events.on "mousedown" (Decode.map DragStart Mouse.position)


width : Int
width =
    1024


height : Int
height =
    768


tileSize : Int
tileSize =
    65


tileSizeFloat : Float
tileSizeFloat =
    toFloat tileSize


makeFlatTerrainMesh : Mesh Vertex
makeFlatTerrainMesh =
    let
        vertices =
            terrainVertices tileSize tileSize

        indices =
            terrainIndices tileSize tileSize
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
            { aPosition = Vec3.vec3 (toFloat x) 0 (toFloat z) }


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
            | uPerspective : Mat4
            , uView : Mat4
            , uWorld : Mat4
            , uWorldOffset : Vec2
        }
        { vColor : Vec3 }
terrainVertexShader =
    [glsl|
precision mediump float;

attribute vec3 aPosition;

uniform vec2 uWorldOffset;
uniform mat4 uPerspective;
uniform mat4 uView;
uniform mat4 uWorld;

varying vec3 vColor;

vec3 ambientLightColor = vec3(1.0);
float ambientLightStrength = 0.2;

//vec3 sunLightColor = vec3(182.0 / 255.0, 126.0 / 255.0, 91.0 / 255.0);
vec3 sunLightColor = vec3(1.0);
vec3 sunDirection = normalize(vec3(1.0, 1.0, 0.0));

vec3 ambientLight();
vec3 sunLight(vec3 normal);

float generateHeight(vec3 pos);

vec3 mod289(vec3 x);
vec2 mod289(vec2 x);
vec3 permute(vec3 x);
float snoise(vec2 v);

void main()
{
    vec3 position = (uWorld * vec4(aPosition, 1.0)).xyz;

    vec3 worldOffset = vec3(uWorldOffset.x, 0.0, uWorldOffset.y);

    vec3 v0 = position + vec3(0.0, 0.0, -1.0);
    v0.y = generateHeight(v0 + worldOffset);

    vec3 v1 = position + vec3(1.0, 0.0, -1.0);
    v1.y = generateHeight(v1 + worldOffset);

    vec3 v2 = position + vec3(-1.0, 0.0, 0.0);
    v2.y = generateHeight(v2 + worldOffset);

    vec3 current = position;
    current.y = generateHeight(position + worldOffset);

    vec3 v3 = position + vec3(1.0, 0.0, 0.0);
    v3.y = generateHeight(v3 + worldOffset);

    vec3 v4 = position + vec3(-1.0, 0.0, 1.0);
    v4.y = generateHeight(v4 + worldOffset);

    vec3 v5 = position + vec3(0.0, 0.0, 1.0);
    v5.y = generateHeight(v5 + worldOffset);

    vec3 norm1 = normalize(cross(v0 - v2, v0 - current));
    vec3 norm2 = normalize(cross(v1 - v0, v1 - current));
    vec3 norm3 = normalize(cross(v1 - current, v1 - v3));
    vec3 norm4 = normalize(cross(current - v2, current - v4));
    vec3 norm5 = normalize(cross(current - v4, current - v5));
    vec3 norm6 = normalize(cross(v3 - current, v3 - v5));

    vec3 normal = normalize(norm1 + norm2 + norm3 + norm4 + norm5 + norm6);

    vColor = vec3(0.3) * (ambientLight() + sunLight(normal));

    gl_Position = uPerspective * uView * vec4(current, 1.0);
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

float generateHeight(vec3 pos)
{
    float dividend = 256.0;
    vec2 inp = vec2(pos.x / dividend, pos.z / dividend) * 2.0;
    float h = snoise(inp) * 20.0;

    return h;
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
        , subscriptions = subscriptions
        }
