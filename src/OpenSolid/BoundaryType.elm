module OpenSolid.BoundaryType exposing (exterior, interior)

import OpenSolid.Parametric.Implementation as Implementation
import OpenSolid.Parametric.Types exposing (..)


interior : BoundaryType
interior =
    Implementation.Interior


exterior : BoundaryType
exterior =
    Implementation.Exterior
