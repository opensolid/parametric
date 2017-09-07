module OpenSolid.Body3d
    exposing
        ( Body3d
        , extrusion
        , fromSurfaces
        , rotateAround
        , surfaces
        , translateBy
        )

import OpenSolid.Axis3d as Axis3d exposing (Axis3d)
import OpenSolid.Parametric.Implementation as Implementation
import OpenSolid.Parametric.Types as Types exposing (..)
import OpenSolid.SketchPlane3d as SketchPlane3d exposing (SketchPlane3d)
import OpenSolid.Vector3d as Vector3d exposing (Vector3d)


type alias Body3d =
    Types.Body3d


extrusion : Region2d -> SketchPlane3d -> Float -> Body3d
extrusion =
    Implementation.body3dExtrusion


fromSurfaces : List Surface3d -> Body3d
fromSurfaces =
    Implementation.Body3d


surfaces : Body3d -> List Surface3d
surfaces =
    Implementation.body3dSurfaces


translateBy : Vector3d -> Body3d -> Body3d
translateBy =
    Implementation.body3dTranslateBy


rotateAround : Axis3d -> Float -> Body3d -> Body3d
rotateAround =
    Implementation.body3dRotateAround
