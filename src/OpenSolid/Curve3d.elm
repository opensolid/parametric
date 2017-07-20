module OpenSolid.Curve3d
    exposing
        ( arc
        , cubicSpline
        , lineSegment
        , point
        , projectOnto
        , quadraticSpline
        )

import OpenSolid.Geometry.Types exposing (..)
import OpenSolid.Parametric.Implementation as Implementation
import OpenSolid.Parametric.Types exposing (..)


lineSegment : LineSegment3d -> Curve3d
lineSegment =
    Implementation.LineSegment3dCurve


arc : Arc3d -> Curve3d
arc =
    Implementation.Arc3dCurve


cubicSpline : CubicSpline3d -> Curve3d
cubicSpline =
    Implementation.CubicSpline3dCurve


quadraticSpline : QuadraticSpline3d -> Curve3d
quadraticSpline =
    Implementation.QuadraticSpline3dCurve


point : Curve3d -> Float -> Point3d
point =
    Implementation.curve3dPoint


projectOnto : Plane3d -> Curve3d -> Curve3d
projectOnto =
    Implementation.curve3dProjectOnto
