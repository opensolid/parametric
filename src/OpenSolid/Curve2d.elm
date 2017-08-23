module OpenSolid.Curve2d
    exposing
        ( Clause
        , If
        , arc
        , cubicSpline
        , elseIf
        , else_
        , endPoint
        , if_
        , isArc
        , isCubicSpline
        , isLineSegment
        , isQuadraticSpline
        , lineSegment
        , mirrorAcross
        , placeIn
        , placeOnto
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
import OpenSolid.Parametric.Implementation as Implementation
import OpenSolid.Parametric.Types exposing (..)
import OpenSolid.Point2d as Point2d exposing (Point2d)
import OpenSolid.Polyline2d as Polyline2d exposing (Polyline2d)
import OpenSolid.QuadraticSpline2d as QuadraticSpline2d exposing (QuadraticSpline2d)
import OpenSolid.SketchPlane3d as SketchPlane3d exposing (SketchPlane3d)
import OpenSolid.Vector2d as Vector2d exposing (Vector2d)


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


toPolyline : Float -> Curve2d -> Polyline2d
toPolyline =
    Implementation.curve2dToPolyline


translateBy : Vector2d -> Curve2d -> Curve2d
translateBy =
    Implementation.curve2dTranslateBy


rotateAround : Point2d -> Float -> Curve2d -> Curve2d
rotateAround =
    Implementation.curve2dRotateAround


mirrorAcross : Axis2d -> Curve2d -> Curve2d
mirrorAcross =
    Implementation.curve2dMirrorAcross


startPoint : Curve2d -> Point2d
startPoint =
    Implementation.curve2dStartPoint


endPoint : Curve2d -> Point2d
endPoint =
    Implementation.curve2dEndPoint


reverse : Curve2d -> Curve2d
reverse =
    Implementation.curve2dReverse


type If a
    = If (List (Clause a))


type Clause a
    = ArcClause (Arc2d -> a)
    | LineSegmentClause (LineSegment2d -> a)
    | QuadraticSplineClause (QuadraticSpline2d -> a)
    | CubicSplineClause (CubicSpline2d -> a)


if_ : Clause a -> If a
if_ clause =
    If [ clause ]


elseIf : Clause a -> If a -> If a
elseIf clause (If clauses) =
    If (clause :: clauses)


else_ : (Curve2d -> a) -> If a -> Curve2d -> a
else_ fallback (If clauses) =
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

        updateHandlers clause handlers =
            case clause of
                ArcClause handler ->
                    { handlers | handleArc = wrap handler }

                LineSegmentClause handler ->
                    { handlers | handleLineSegment = wrap handler }

                QuadraticSplineClause handler ->
                    { handlers | handleQuadraticSpline = wrap handler }

                CubicSplineClause handler ->
                    { handlers | handleCubicSpline = wrap handler }

        { handleArc, handleLineSegment, handleQuadraticSpline, handleCubicSpline } =
            List.foldl updateHandlers initialHandlers clauses
    in
    \curve ->
        case curve of
            Implementation.LineSegment2dCurve lineSegment ->
                handleLineSegment lineSegment curve

            Implementation.Arc2dCurve arc ->
                handleArc arc curve

            Implementation.QuadraticSpline2dCurve quadraticSpline ->
                handleQuadraticSpline quadraticSpline curve

            Implementation.CubicSpline2dCurve cubicSpline ->
                handleCubicSpline cubicSpline curve

            _ ->
                fallback curve


isLineSegment : (LineSegment2d -> a) -> Clause a
isLineSegment =
    LineSegmentClause


isArc : (Arc2d -> a) -> Clause a
isArc =
    ArcClause


isQuadraticSpline : (QuadraticSpline2d -> a) -> Clause a
isQuadraticSpline =
    QuadraticSplineClause


isCubicSpline : (CubicSpline2d -> a) -> Clause a
isCubicSpline =
    CubicSplineClause
