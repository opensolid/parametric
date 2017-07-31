module OpenSolid.Surface3d
    exposing
        ( pointOn
        )

import OpenSolid.Geometry.Types exposing (..)
import OpenSolid.Parametric.Implementation as Implementation
import OpenSolid.Parametric.Types exposing (..)


pointOn : Surface3d -> Point2d -> Point3d
pointOn =
    Implementation.surface3dPointOn
