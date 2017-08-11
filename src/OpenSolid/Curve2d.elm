module OpenSolid.Curve2d
    exposing
        ( arc
        , cubicSpline
        , fromArc
        , fromCubicSpline
        , fromLineSegment
        , fromQuadraticSpline
        , lineSegment
        , placeIn
        , placeOnto
        , pointOn
        , quadraticSpline
        , relativeTo
        , rotateAround
        , toPolyline
        , translateBy
        )

import OpenSolid.Geometry.Types exposing (..)
import OpenSolid.Parametric.Implementation as Implementation
import OpenSolid.Parametric.Types exposing (..)


fromLineSegment : LineSegment2d -> Curve2d
fromLineSegment =
    Implementation.LineSegment2dCurve


lineSegment : ( Point2d, Point2d ) -> Curve2d
lineSegment =
    fromLineSegment << LineSegment2d


fromArc : Arc2d -> Curve2d
fromArc =
    Implementation.Arc2dCurve


arc : { startPoint : Point2d, centerPoint : Point2d, sweptAngle : Float } -> Curve2d
arc =
    fromArc << Arc2d


fromCubicSpline : CubicSpline2d -> Curve2d
fromCubicSpline =
    Implementation.CubicSpline2dCurve


cubicSpline : ( Point2d, Point2d, Point2d, Point2d ) -> Curve2d
cubicSpline =
    fromCubicSpline << CubicSpline2d


fromQuadraticSpline : QuadraticSpline2d -> Curve2d
fromQuadraticSpline =
    Implementation.QuadraticSpline2dCurve


quadraticSpline : ( Point2d, Point2d, Point2d ) -> Curve2d
quadraticSpline =
    fromQuadraticSpline << QuadraticSpline2d


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


toPolyline : Float -> Curve2d -> Polyline2d
toPolyline =
    Implementation.curve2dToPolyline


translateBy : Vector2d -> Curve2d -> Curve2d
translateBy =
    Implementation.curve2dTranslateBy


rotateAround : Point2d -> Float -> Curve2d -> Curve2d
rotateAround =
    Implementation.curve2dRotateAround
