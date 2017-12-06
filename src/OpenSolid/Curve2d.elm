module OpenSolid.Curve2d
    exposing
        ( Curve2d
        , Matcher
        , arc
        , cubicSpline
        , endPoint
        , isArc
        , isCubicSpline
        , isLineSegment
        , isQuadraticSpline
        , lineSegment
        , match
        , mirrorAcross
        , otherwise
        , placeIn
        , pointOn
        , quadraticSpline
        , relativeTo
        , reverse
        , rotateAround
        , startPoint
        , toPolyline
        , translateBy
        )

import OpenSolid.Arc2d as Arc2d exposing (Arc2d)
import OpenSolid.Axis2d as Axis2d exposing (Axis2d)
import OpenSolid.CubicSpline2d as CubicSpline2d exposing (CubicSpline2d)
import OpenSolid.Frame2d as Frame2d exposing (Frame2d)
import OpenSolid.LineSegment2d as LineSegment2d exposing (LineSegment2d)
import OpenSolid.Parametric.Curve as Curve
import OpenSolid.Point2d as Point2d exposing (Point2d)
import OpenSolid.Polyline2d as Polyline2d exposing (Polyline2d)
import OpenSolid.QuadraticSpline2d as QuadraticSpline2d exposing (QuadraticSpline2d)
import OpenSolid.Vector2d as Vector2d exposing (Vector2d)


type alias Curve2d =
    Curve.Curve2d


lineSegment : LineSegment2d -> Curve2d
lineSegment =
    Curve.LineSegment2dCurve


arc : Arc2d -> Curve2d
arc =
    Curve.Arc2dCurve


cubicSpline : CubicSpline2d -> Curve2d
cubicSpline =
    Curve.CubicSpline2dCurve


quadraticSpline : QuadraticSpline2d -> Curve2d
quadraticSpline =
    Curve.QuadraticSpline2dCurve


pointOn : Curve2d -> Float -> Point2d
pointOn =
    Curve.pointOn2d


relativeTo : Frame2d -> Curve2d -> Curve2d
relativeTo =
    Curve.relativeTo2d


placeIn : Frame2d -> Curve2d -> Curve2d
placeIn =
    Curve.placeIn2d


toPolyline : Float -> Curve2d -> Polyline2d
toPolyline =
    Curve.toPolyline2d


translateBy : Vector2d -> Curve2d -> Curve2d
translateBy =
    Curve.translateBy2d


rotateAround : Point2d -> Float -> Curve2d -> Curve2d
rotateAround =
    Curve.rotateAround2d


mirrorAcross : Axis2d -> Curve2d -> Curve2d
mirrorAcross =
    Curve.mirrorAcross2d


startPoint : Curve2d -> Point2d
startPoint =
    Curve.startPoint2d


endPoint : Curve2d -> Point2d
endPoint =
    Curve.endPoint2d


reverse : Curve2d -> Curve2d
reverse =
    Curve.reverse2d


type Matcher a
    = Matcher (List (Case a))


type Case a
    = ArcCase (Arc2d -> a)
    | LineSegmentCase (LineSegment2d -> a)
    | QuadraticSplineCase (QuadraticSpline2d -> a)
    | CubicSplineCase (CubicSpline2d -> a)


match : Matcher a
match =
    Matcher []


isLineSegment : (LineSegment2d -> a) -> Matcher a -> Matcher a
isLineSegment handler (Matcher cases) =
    Matcher (LineSegmentCase handler :: cases)


isArc : (Arc2d -> a) -> Matcher a -> Matcher a
isArc handler (Matcher cases) =
    Matcher (ArcCase handler :: cases)


isQuadraticSpline : (QuadraticSpline2d -> a) -> Matcher a -> Matcher a
isQuadraticSpline handler (Matcher cases) =
    Matcher (QuadraticSplineCase handler :: cases)


isCubicSpline : (CubicSpline2d -> a) -> Matcher a -> Matcher a
isCubicSpline handler (Matcher cases) =
    Matcher (CubicSplineCase handler :: cases)


otherwise : (Curve2d -> a) -> Matcher a -> Curve2d -> a
otherwise fallback (Matcher cases) =
    let
        defaultHandler _ curve =
            fallback curve

        initialHandlers =
            { handleArc = defaultHandler
            , handleLineSegment = defaultHandler
            , handleQuadraticSpline = defaultHandler
            , handleCubicSpline = defaultHandler
            }

        wrap handler argument _ =
            handler argument

        updateHandlers case_ handlers =
            case case_ of
                ArcCase handler ->
                    { handlers | handleArc = wrap handler }

                LineSegmentCase handler ->
                    { handlers | handleLineSegment = wrap handler }

                QuadraticSplineCase handler ->
                    { handlers | handleQuadraticSpline = wrap handler }

                CubicSplineCase handler ->
                    { handlers | handleCubicSpline = wrap handler }

        { handleArc, handleLineSegment, handleQuadraticSpline, handleCubicSpline } =
            List.foldl updateHandlers initialHandlers cases
    in
    \curve ->
        case curve of
            Curve.LineSegment2dCurve lineSegment ->
                handleLineSegment lineSegment curve

            Curve.Arc2dCurve arc ->
                handleArc arc curve

            Curve.QuadraticSpline2dCurve quadraticSpline ->
                handleQuadraticSpline quadraticSpline curve

            Curve.CubicSpline2dCurve cubicSpline ->
                handleCubicSpline cubicSpline curve

            _ ->
                fallback curve
