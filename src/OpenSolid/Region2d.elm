module OpenSolid.Region2d
    exposing
        ( EdgeType
        , boundaries
        , exterior
        , fromRectangle
        , fromRectangleWith
        , interior
        , rectangle
        , rectangleWith
        )

import OpenSolid.Geometry.Types exposing (..)
import OpenSolid.Parametric.Implementation as Implementation
import OpenSolid.Parametric.Types exposing (..)
import OpenSolid.Rectangle2d as Rectangle2d


type alias EdgeType =
    Implementation.EdgeType


interior : EdgeType
interior =
    Implementation.Interior


exterior : EdgeType
exterior =
    Implementation.Exterior


rectangle : { minX : Float, maxX : Float, minY : Float, maxY : Float } -> Region2d
rectangle extrema =
    fromRectangle (Rectangle2d.with extrema)


rectangleWith : { left : EdgeType, right : EdgeType, top : EdgeType, bottom : EdgeType } -> { minX : Float, maxX : Float, minY : Float, maxY : Float } -> Region2d
rectangleWith edgeTypes extrema =
    fromRectangleWith edgeTypes (Rectangle2d.with extrema)


fromRectangle : Rectangle2d -> Region2d
fromRectangle =
    fromRectangleWith
        { left = exterior
        , right = exterior
        , top = exterior
        , bottom = exterior
        }


fromRectangleWith : { left : EdgeType, right : EdgeType, top : EdgeType, bottom : EdgeType } -> Rectangle2d -> Region2d
fromRectangleWith edgeTypes rectangle =
    Implementation.RectangleRegion rectangle edgeTypes


boundaries : Region2d -> List Curve2d
boundaries =
    Implementation.regionBoundaries
