module OpenSolid.Surface3d
    exposing
        ( Surface3d
        , extrusion
        , planar
        , pointOn
        , revolution
        , rotateAround
        , toMesh
        , translateBy
        )

import OpenSolid.Axis3d as Axis3d exposing (Axis3d)
import OpenSolid.Mesh exposing (Mesh)
import OpenSolid.Parametric.Implementation as Implementation
import OpenSolid.Parametric.Types as Types exposing (..)
import OpenSolid.Point2d as Point2d exposing (Point2d)
import OpenSolid.Point3d as Point3d exposing (Point3d)
import OpenSolid.SketchPlane3d as SketchPlane3d exposing (SketchPlane3d)
import OpenSolid.Vector3d as Vector3d exposing (Vector3d)


type alias Surface3d =
    Types.Surface3d


pointOn : Surface3d -> Point2d -> Point3d
pointOn =
    Implementation.surface3dPointOn


toMesh : Float -> Surface3d -> Mesh ( Point3d, Vector3d )
toMesh =
    Implementation.surface3dToMesh


translateBy : Vector3d -> Surface3d -> Surface3d
translateBy =
    Implementation.surface3dTranslateBy


rotateAround : Axis3d -> Float -> Surface3d -> Surface3d
rotateAround =
    Implementation.surface3dRotateAround


planar : Region2d -> SketchPlane3d -> Surface3d
planar =
    Implementation.surface3dPlanar


extrusion : Curve3d -> Vector3d -> Surface3d
extrusion =
    Implementation.surface3dExtrusion


revolution : Curve3d -> Axis3d -> Float -> Surface3d
revolution =
    Implementation.surface3dRevolution


flip : Surface3d -> Surface3d
flip =
    Implementation.surface3dFlip
