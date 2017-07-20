module OpenSolid.Curve2d
    exposing
        ( arc
        , cubicSpline
        , lineSegment
        , placeOnto
        , point
        , quadraticSpline
        )

import OpenSolid.Geometry.Types exposing (..)
import OpenSolid.Parametric.Implementation as Implementation
import OpenSolid.Parametric.Types exposing (..)


lineSegment : LineSegment2d -> Curve2d
lineSegment =
    Implementation.LineSegment2dCurve


arc : Arc2d -> Curve2d
arc =
    Implementation.Arc2dCurve


cubicSpline : CubicSpline2d -> Curve2d
cubicSpline =
    Implementation.CubicSpline2dCurve


quadraticSpline : QuadraticSpline2d -> Curve2d
quadraticSpline =
    Implementation.QuadraticSpline2dCurve


point : Curve2d -> Float -> Point2d
point =
    Implementation.curve2dPoint


placeOnto : SketchPlane3d -> Curve2d -> Curve3d
placeOnto =
    Implementation.curve2dPlaceOnto
