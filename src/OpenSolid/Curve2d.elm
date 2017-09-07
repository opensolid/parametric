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
import OpenSolid.Parametric.Implementation as Implementation
import OpenSolid.Parametric.Types as Types exposing (..)
import OpenSolid.Point2d as Point2d exposing (Point2d)
import OpenSolid.Polyline2d as Polyline2d exposing (Polyline2d)
import OpenSolid.QuadraticSpline2d as QuadraticSpline2d exposing (QuadraticSpline2d)
import OpenSolid.Vector2d as Vector2d exposing (Vector2d)


type alias Curve2d =
    Types.Curve2d


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
