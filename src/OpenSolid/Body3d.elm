module OpenSolid.Body3d
    exposing
        ( extrusion
        )

import OpenSolid.Geometry.Types exposing (..)
import OpenSolid.Parametric.Implementation as Implementation
import OpenSolid.Parametric.Types exposing (..)


extrusion : Region2d -> SketchPlane3d -> Float -> Body3d
extrusion =
    Implementation.body3dExtrusion
