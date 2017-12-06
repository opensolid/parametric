module OpenSolid.Curve3d
    exposing
        ( arc
        , cubicSpline
        , endPoint
        , lineSegment
        , on
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
import OpenSolid.Parametric.Curve as Curve
import OpenSolid.Plane3d as Plane3d exposing (Plane3d)
import OpenSolid.Point3d as Point3d exposing (Point3d)
import OpenSolid.Polyline3d as Polyline3d exposing (Polyline3d)
import OpenSolid.QuadraticSpline3d as QuadraticSpline3d exposing (QuadraticSpline3d)
import OpenSolid.SketchPlane3d as SketchPlane3d exposing (SketchPlane3d)
import OpenSolid.Vector3d as Vector3d exposing (Vector3d)


type alias Curve3d =
    Curve.Curve3d


lineSegment : LineSegment3d -> Curve3d
lineSegment =
    Curve.LineSegment3dCurve


arc : Arc3d -> Curve3d
arc =
    Curve.Arc3dCurve


cubicSpline : CubicSpline3d -> Curve3d
cubicSpline =
    Curve.CubicSpline3dCurve


quadraticSpline : QuadraticSpline3d -> Curve3d
quadraticSpline =
    Curve.QuadraticSpline3dCurve


on : SketchPlane3d -> Curve.Curve2d -> Curve3d
on =
    Curve.on3d


pointOn : Curve3d -> Float -> Point3d
pointOn =
    Curve.pointOn3d


projectOnto : Plane3d -> Curve3d -> Curve3d
projectOnto =
    Curve.projectOnto


relativeTo : Frame3d -> Curve3d -> Curve3d
relativeTo =
    Curve.relativeTo3d


placeIn : Frame3d -> Curve3d -> Curve3d
placeIn =
    Curve.placeIn3d


toPolyline : Float -> Curve3d -> Polyline3d
toPolyline =
    Curve.toPolyline3d


rotateAround : Axis3d -> Float -> Curve3d -> Curve3d
rotateAround =
    Curve.rotateAround3d


translateBy : Vector3d -> Curve3d -> Curve3d
translateBy =
    Curve.translateBy3d


startPoint : Curve3d -> Point3d
startPoint =
    Curve.startPoint3d


endPoint : Curve3d -> Point3d
endPoint =
    Curve.endPoint3d


reverse : Curve3d -> Curve3d
reverse =
    Curve.reverse3d
