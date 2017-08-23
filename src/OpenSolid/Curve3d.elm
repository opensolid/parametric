module OpenSolid.Curve3d
    exposing
        ( arc
        , cubicSpline
        , endPoint
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

import OpenSolid.Arc3d as Arc3d exposing (Arc3d)
import OpenSolid.Axis3d as Axis3d exposing (Axis3d)
import OpenSolid.CubicSpline3d as CubicSpline3d exposing (CubicSpline3d)
import OpenSolid.Frame3d as Frame3d exposing (Frame3d)
import OpenSolid.LineSegment3d as LineSegment3d exposing (LineSegment3d)
import OpenSolid.Parametric.Implementation as Implementation
import OpenSolid.Parametric.Types exposing (..)
import OpenSolid.Plane3d as Plane3d exposing (Plane3d)
import OpenSolid.Point3d as Point3d exposing (Point3d)
import OpenSolid.Polyline3d as Polyline3d exposing (Polyline3d)
import OpenSolid.QuadraticSpline3d as QuadraticSpline3d exposing (QuadraticSpline3d)
import OpenSolid.Vector3d as Vector3d exposing (Vector3d)


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
