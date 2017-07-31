module OpenSolid.Curve3d
    exposing
        ( arc
        , cubicSpline
        , extrudeBy
        , lineSegment
        , placeIn
        , pointOn
        , projectOnto
        , quadraticSpline
        , relativeTo
        , revolveAround
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


pointOn : Curve3d -> Float -> Point3d
pointOn =
    Implementation.curve3dPointOn


projectOnto : Plane3d -> Curve3d -> Curve3d
projectOnto =
    Implementation.curve3dProjectOnto


relativeTo : Frame3d -> Curve3d -> Curve3d
relativeTo =
    Implementation.curve3dRelativeTo


placeIn : Frame3d -> Curve3d -> Curve3d
placeIn =
    Implementation.curve3dPlaceIn


extrudeBy : Vector3d -> Curve3d -> Surface3d
extrudeBy =
    Implementation.curve3dExtrudeBy


revolveAround : Axis3d -> Float -> Curve3d -> Surface3d
revolveAround =
    Implementation.curve3dRevolveAround
