module OpenSolid.Body3d
    exposing
        ( Body3d
        , extrusion
        , fromSurfaces
        , surfaces
        )

import OpenSolid.Parametric.Implementation as Implementation
import OpenSolid.Parametric.Types as Types exposing (..)
import OpenSolid.SketchPlane3d as SketchPlane3d exposing (SketchPlane3d)


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
