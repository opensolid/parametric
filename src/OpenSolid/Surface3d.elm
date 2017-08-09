module OpenSolid.Surface3d
    exposing
        ( pointOn
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
