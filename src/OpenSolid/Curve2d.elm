module OpenSolid.Curve2d
    exposing
        ( arc
        , cubicSpline
        , lineSegment
        , placeIn
        , placeOnto
        , pointOn
        , quadraticSpline
        , relativeTo
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


pointOn : Curve2d -> Float -> Point2d
pointOn =
    Implementation.curve2dPointOn


placeOnto : SketchPlane3d -> Curve2d -> Curve3d
placeOnto =
    Implementation.curve2dPlaceOnto


relativeTo : Frame2d -> Curve2d -> Curve2d
relativeTo =
    Implementation.curve2dRelativeTo


placeIn : Frame2d -> Curve2d -> Curve2d
placeIn =
    Implementation.curve2dPlaceIn
