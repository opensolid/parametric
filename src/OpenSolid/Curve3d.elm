module OpenSolid.Curve3d
    exposing
        ( arc
        , cubicSpline
        , extrudeBy
        , fromArc
        , fromCubicSpline
        , fromLineSegment
        , fromQuadraticSpline
        , lineSegment
        , placeIn
        , pointOn
        , projectOnto
        , quadraticSpline
        , relativeTo
        , revolveAround
        , toPolyline
        )

import OpenSolid.Geometry.Types exposing (..)
import OpenSolid.Parametric.Implementation as Implementation
import OpenSolid.Parametric.Types exposing (..)


lineSegment : ( Point3d, Point3d ) -> Curve3d
lineSegment =
    fromLineSegment << LineSegment3d


fromLineSegment : LineSegment3d -> Curve3d
fromLineSegment =
    Implementation.LineSegment3dCurve


arc : { startPoint : Point3d, axis : Axis3d, sweptAngle : Float } -> Curve3d
arc =
    fromArc << Arc3d


fromArc : Arc3d -> Curve3d
fromArc =
    Implementation.Arc3dCurve


cubicSpline : ( Point3d, Point3d, Point3d, Point3d ) -> Curve3d
cubicSpline =
    fromCubicSpline << CubicSpline3d


fromCubicSpline : CubicSpline3d -> Curve3d
fromCubicSpline =
    Implementation.CubicSpline3dCurve


quadraticSpline : ( Point3d, Point3d, Point3d ) -> Curve3d
quadraticSpline =
    fromQuadraticSpline << QuadraticSpline3d


fromQuadraticSpline : QuadraticSpline3d -> Curve3d
fromQuadraticSpline =
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


toPolyline : Float -> Curve3d -> Polyline3d
toPolyline =
    Implementation.curve3dToPolyline
