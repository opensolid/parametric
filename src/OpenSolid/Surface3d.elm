module OpenSolid.Surface3d
    exposing
        ( extrusion
        , planar
        , pointOn
        , revolution
        , rotateAround
        , toMesh
        )

import OpenSolid.Geometry.Types exposing (..)
import OpenSolid.Mesh exposing (Mesh)
import OpenSolid.Parametric.Implementation as Implementation
import OpenSolid.Parametric.Types exposing (..)


pointOn : Surface3d -> Point2d -> Point3d
pointOn =
    Implementation.surface3dPointOn


toMesh : Float -> Surface3d -> Mesh ( Point3d, Vector3d )
toMesh =
    Implementation.surface3dToMesh


rotateAround : Axis3d -> Float -> Surface3d -> Surface3d
rotateAround =
    Implementation.surface3dRotateAround


planar : Region2d -> SketchPlane3d -> Surface3d
planar =
    Implementation.PlanarSurface


extrusion : Curve3d -> Vector3d -> Surface3d
extrusion =
    Implementation.surface3dExtrusion


revolution : Curve3d -> Axis3d -> Float -> Surface3d
revolution =
    Implementation.surface3dRevolution
