module OpenSolid.Body3d
    exposing
        ( extrusion
        , fromSurfaces
        , surfaces
        )

import OpenSolid.Geometry.Types exposing (..)
import OpenSolid.Parametric.Implementation as Implementation
import OpenSolid.Parametric.Types exposing (..)


extrusion : Region2d -> SketchPlane3d -> Float -> Body3d
extrusion =
    Implementation.body3dExtrusion


fromSurfaces : List Surface3d -> Body3d
fromSurfaces =
    Implementation.Body3d


surfaces : Body3d -> List Surface3d
surfaces =
    Implementation.body3dSurfaces
