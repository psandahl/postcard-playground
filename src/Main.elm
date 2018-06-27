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

void main()
{
    vec3 normal = vec3(0.0, 1.0, 0.0);

    vColor = vec3(0.3) * (ambientLight() + sunLight(normal));

    mat4 mvp = perspective * view;
    gl_Position = mvp * vec4(position, 1.0);
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
