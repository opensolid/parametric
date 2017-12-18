module OpenSolid.BoundaryType exposing (BoundaryType, exterior, interior)

import OpenSolid.Parametric.Implementation as Implementation
import OpenSolid.Parametric.Types as Types


type alias BoundaryType =
    Types.BoundaryType


interior : BoundaryType
interior =
    Implementation.Interior


exterior : BoundaryType
exterior =
    Implementation.Exterior
