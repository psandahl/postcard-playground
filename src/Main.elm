module Main exposing (main)

import Html exposing (Html)
import Html.Attributes as Attr
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
      , view = Mat4.makeLookAt (Vec3.vec3 0 1 0) (Vec3.vec3 1 0 1) (Vec3.vec3 0 1 0)
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
            terrainVertex
            terrainFragment
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
            [ { position = Vec3.vec3 0 0 0 }
            , { position = Vec3.vec3 1 0 0 }
            , { position = Vec3.vec3 0 0 1 }
            , { position = Vec3.vec3 1 0 1 }
            ]

        indices =
            [ ( 1, 0, 2 ), ( 1, 2, 3 ) ]
    in
    GL.indexedTriangles vertices indices


terrainVertex : Shader Vertex { uniforms | perspective : Mat4, view : Mat4 } {}
terrainVertex =
    [glsl|
precision mediump float;

attribute vec3 position;

uniform mat4 perspective;
uniform mat4 view;

void main()
{
    mat4 mvp = perspective * view;
    gl_Position = mvp * vec4(position, 1.0);
}
    |]


terrainFragment : Shader {} uniforms {}
terrainFragment =
    [glsl|
precision mediump float;

void main()
{
    gl_FragColor = vec4(1.0, 0.0, 0.0, 1.0);
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
