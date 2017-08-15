module OpenSolid.Curve3d
    exposing
        ( arc
        , cubicSpline
        , endPoint
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
        , reverse
        , rotateAround
        , startPoint
        , toPolyline
        , translateBy
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


toPolyline : Float -> Curve3d -> Polyline3d
toPolyline =
    Implementation.curve3dToPolyline


rotateAround : Axis3d -> Float -> Curve3d -> Curve3d
rotateAround =
    Implementation.curve3dRotateAround


translateBy : Vector3d -> Curve3d -> Curve3d
translateBy =
    Implementation.curve3dTranslateBy


startPoint : Curve3d -> Point3d
startPoint =
    Implementation.curve3dStartPoint


endPoint : Curve3d -> Point3d
endPoint =
    Implementation.curve3dEndPoint


reverse : Curve3d -> Curve3d
reverse =
    Implementation.curve3dReverse
